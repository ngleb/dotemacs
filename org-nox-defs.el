;;; Entry

(defun +agenda-is-low-effort (effort-text &optional todo-type)
  (and effort-text
       (or (not todo-type) (eq todo-type 'todo))
       (< (org-duration-to-minutes effort-text) 20)))

(cl-defstruct +agenda-entry todo priority text tags planned low-effort marker project-status children)

(defun +agenda-entry (headline &optional tags)
  (let ((todo-type (org-element-property :todo-type headline))
        (effort (org-element-property :EFFORT headline)))
    (make-+agenda-entry
     :todo (org-element-property :todo-keyword headline)
     :priority (org-element-property :priority headline)
     :text (org-element-property :raw-value headline)
     :tags (or tags (org-element-property :tags headline))
     :low-effort (+agenda-is-low-effort effort todo-type)
     :marker (org-agenda-new-marker (org-element-property :begin headline)))))


;;; Renderer

(defconst +agenda-projects-not-task-faces '(("NEXT" . (:inherit org-todo :weight normal))
                                            ("TODO" . (:inherit org-todo :weight normal))))

(defconst +agenda-projects-task-faces '(("NEXT" . (:inherit org-todo :weight bold))
                                        ("TODO" . (:inherit org-todo :weight bold))))

(defun +agenda-format-entry (prefix entry)
  (let ((props (list 'nox-custom-agenda t
                     'mouse-face 'highlight
                     'undone-face nil
                     'done-face 'org-agenda-done
                     'org-marker (+agenda-entry-marker entry)
                     'org-hd-marker (+agenda-entry-marker entry)
                     'todo-state (+agenda-entry-todo entry)
                     'org-todo-regexp org-todo-regexp
                     'org-not-done-regexp org-not-done-regexp
                     'org-complex-heading-regexp org-complex-heading-regexp
                     'org-highest-priority org-highest-priority
                     'org-lowest-priority org-lowest-priority
	                 'tags (mapcar 'org-downcase-keep-props (+agenda-entry-tags entry))
	                 'format `(() ,prefix)))
        (text
         (concat prefix
                 (if (+agenda-entry-todo entry)
                     (concat (+agenda-entry-todo entry) " ")
                   "")
                 (if (+agenda-entry-priority entry)
                     (string ?\[ ?# (+agenda-entry-priority entry) ?\] ? )
                   "")
                 (+agenda-entry-text entry)
                 (if (+agenda-entry-tags entry)
                     (concat " :" (mapconcat #'identity (+agenda-entry-tags entry) ":") ":")
                   ""))))

	(add-text-properties (length prefix) (length text) '(org-heading t) text)
    (setq text (concat (org-add-props text props) "\n"))
    (org-agenda-highlight-todo text)))

(defun +agenda-tip-for-effort (text low-effort &optional alt-text)
  (if low-effort
      (propertize text 'face '(:foreground "#b58900"))
    (or alt-text text)))

(defun +agenda-schedule-get-prefix ()
  (+agenda-tip-for-effort "➤" (+agenda-is-low-effort effort) " "))

(defun +agenda-project-get-prefix (taskp parent-continuations &optional low-effort)
  ;; IMPORTANT(nox): `parent-continuations' is in reverse order!
  (let ((prefix "")
        (tip t))
    (if taskp
        (dolist (cont parent-continuations)
          (setq prefix (concat (if tip
                                   (+agenda-tip-for-effort (if cont "├➤ " "╰➤ ") low-effort)
                                 (if cont "│  " "   "))
                               prefix)
                tip nil))

      (dolist (cont parent-continuations)
        (setq prefix (concat (if tip (if cont "├─╴" "╰─╴") (if cont "│  " "   ")) prefix)
              tip nil)))
    (concat "  " prefix)))

(defun +agenda-priority-sort (a b)
  (let ((pa (or (+agenda-entry-priority a) org-default-priority))
        (pb (or (+agenda-entry-priority b) org-default-priority)))
    (< pa pb)))

(defun gn-org-cmp-tag (a b)
  "Compare the string values of the first tags of A and B."
  (let ((ta (car (last (+agenda-entry-tags a))))
        (tb (car (last (+agenda-entry-tags b)))))
    (string-lessp ta tb)))

(defun +agenda-flatten-list (l)
  (cond ((not l) nil)
        ((atom l) (list l))
        (t (append (+agenda-flatten-list (car l)) (+agenda-flatten-list (cdr l))))))

(defun +agenda-project-printer (list &optional parent-continuations)
  (setq list (sort list #'+agenda-priority-sort))

  (let ((first t) entry)
    (while list
      (setq entry (car list))
      (if parent-continuations
          (unless (cdr list) (setf (car parent-continuations) nil))
        (unless first (insert "\n")))

      (when (eq (+agenda-entry-project-status entry) 'stuck)
        (org-add-props (+agenda-entry-text entry) nil 'face 'org-priority 'nox-face 'org-priority))

      (let ((org-todo-keyword-faces (if (+agenda-entry-project-status entry)
                                        +agenda-projects-not-task-faces
                                      +agenda-projects-task-faces))

            (prefix (+agenda-project-get-prefix (not (+agenda-entry-project-status entry))
                                                parent-continuations
                                                (+agenda-entry-low-effort entry))))
        (insert (+agenda-format-entry prefix entry)))

      (+agenda-project-printer (+agenda-entry-children entry) (cons t parent-continuations))
      (setq list (cdr list)
            first nil))))

(defun +agenda-simple-printer (list)
  ;;(setq list (sort list #'+agenda-priority-sort))
  (setq list (sort list #'gn-org-cmp-tag))
  (dolist (entry list)
    (insert
     (+agenda-format-entry (+agenda-tip-for-effort " ➤" (+agenda-entry-low-effort entry) "  ") entry))))

(defun +agenda-separator ()
  (unless (or (bobp) org-agenda-compact-blocks
			  (not org-agenda-block-separator))
	(insert "\n"
            (if (stringp org-agenda-block-separator)
                org-agenda-block-separator
			  (make-string (window-width) org-agenda-block-separator))
		    "\n")))

(defun +agenda-render-block (data title &optional printer)
  (when data
    (let ((begin (point)))
      (+agenda-separator)
      (insert (org-add-props title nil 'face 'org-agenda-structure) "\n")
      (funcall (or printer #'+agenda-simple-printer) data)
      (add-text-properties begin (point-max) `(org-agenda-type tags)))))


;;; Inbox

(defun +agenda-inbox-process-headline (headline)
  (when (or +agenda-show-private
            (not (member "PRIVATE" (org-element-property :tags headline))))
    (+agenda-entry headline)))

(defun +agenda-inbox (&optional _)
  (catch 'exit
    (let ((files (org-agenda-files nil 'ifmode))
          +agenda-inbox
          org-todo-regexp org-not-done-regexp org-complex-heading-regexp org-done-keywords
          org-done-keywords-for-agenda file buffer ast)
      (while (setq file (pop files))
        (org-check-agenda-file file)
        (setq buffer (if (file-exists-p file)
                         (org-get-agenda-file-buffer file)
                       (error "No such file %s" file)))

        (unless org-todo-regexp
          (dolist (variable '(org-todo-regexp org-not-done-regexp org-complex-heading-regexp
                                              org-done-keywords org-done-keywords-for-agenda))
            (set variable (buffer-local-value variable buffer))))

        (with-current-buffer buffer
          (org-with-wide-buffer
           (unless (derived-mode-p 'org-mode) (error "Agenda file %s is not in Org mode" file))
           (setq ast (org-element-parse-buffer 'headline))
           (setq +agenda-inbox
                 (append (org-element-map ast 'headline #'+agenda-inbox-process-headline nil nil 'headline)
                         +agenda-inbox)))))

      (let ((inhibit-read-only t))
	    (goto-char (point-max))
        (+agenda-render-block +agenda-inbox "Coisas a arrumar")))))


;;; Tasks

(defvar +agenda-level)
(defvar +agenda-parent-tags)
(defvar +agenda-project-status)
(defvar +agenda-projects)
(defvar +agenda-isolated-tasks)
(defvar +agenda-high-priority)
(defvar +agenda-low-priority)
(defvar +agenda-archivable-tasks)
(defvar +agenda-planned-tasks)
(defvar +agenda-hold-tasks)

(defun +agenda-filter-priorities (entry)
  (let ((priority (+agenda-entry-priority entry)))
    (cond ((eq priority ?A) (push entry +agenda-high-priority))
          ((eq priority ?D) (push entry +agenda-low-priority)))))

(defmacro +agenda-process-children (parent &optional task-children)
  (if task-children
      `(let ((+agenda-parent-tags (append (org-element-property :tags ,parent) +agenda-parent-tags))
             (+agenda-level (1+ +agenda-level)))
         (org-element-map (org-element-contents ,parent) 'headline
           #'+agenda-tasks-process-headline nil nil 'headline))
    `(let ((+agenda-parent-tags (append (org-element-property :tags ,parent) +agenda-parent-tags)))
       (org-element-map (org-element-contents ,parent) 'headline #'+agenda-tasks-process-headline
                        nil nil 'headline))))

(defmacro +agenda-set-parent-minimum-status (status)
  `(unless (= +agenda-level 0)
     ,(if (symbolp status)
          (cond ((eq status 'next)    '(setq +agenda-project-status 'next))
                ((eq status 'planned) '(when (not (eq +agenda-project-status 'next))
                                         (setq +agenda-project-status 'planned)))
                (t '(unless +agenda-project-status (setq +agenda-project-status 'stuck))))
        `(cond ((eq ,status 'next)     (setq +agenda-project-status 'next))
               ((eq ,status 'planned)  (when (not (eq +agenda-project-status 'next))
                                         (setq +agenda-project-status 'planned)))
               (t (unless +agenda-project-status (setq +agenda-project-status 'stuck)))))))

(defun gn-timestamp-ts (headline)
  (interactive)
  (let* ((scheduled-ts (org-element-property :raw-value (org-element-property :scheduled headline)))
         (deadline-ts  (org-element-property :raw-value (org-element-property :deadline headline)))
         (contents-begin (org-element-property :contents-begin headline))
         (timestamp (or scheduled-ts deadline-ts))
         (time-to-now (and timestamp (org-time-stamp-to-now timestamp)))
         first-child search-bound temp-time)

    (when (and (not time-to-now) contents-begin)
      (setq first-child (org-element-map (org-element-contents headline) 'headline #'identity
                                         nil t 'headline)
            search-bound (or (and first-child (org-element-property :begin first-child))
                             (org-element-property :end headline)))
      (goto-char contents-begin)
      (when (re-search-forward org-ts-regexp search-bound t)
        (concat "<" (match-string-no-properties 1) ">")))))

(defun +agenda-tasks-process-headline (headline)
  (let* ((todo (org-element-property :todo-keyword headline))
         (todo-type (org-element-property :todo-type headline))
         (scheduled-ts (org-element-property :raw-value (org-element-property :scheduled headline)))
         (deadline-ts  (org-element-property :raw-value (org-element-property :deadline headline)))
         (timestamp-ts (gn-timestamp-ts headline))
         (closed-ts  (org-element-property :raw-value (org-element-property :closed headline)))
         (has-scheduling (or scheduled-ts deadline-ts timestamp-ts))
         (scheduled-future (cond (scheduled-ts (> (org-time-stamp-to-now scheduled-ts) 0))
                                 (deadline-ts  (> (org-time-stamp-to-now deadline-ts)
                                                  (org-get-wdays deadline-ts)))
                                 (timestamp-ts (> (org-time-stamp-to-now timestamp-ts) 0))))
         (scheduled-past-or-now (and has-scheduling (not scheduled-future)))
         (effort (org-element-property :EFFORT headline))
         (contents-begin (org-element-property :contents-begin headline))
         (tickler (member "TICKLER" (org-element-property :tags headline)))
         entry project-status return)

    (when (or +agenda-show-private (not (member "PRIVATE" (org-element-property :tags headline))))
      (setq entry
            (+agenda-entry headline (cl-remove-duplicates
                                     (append (org-element-property :tags headline) +agenda-parent-tags)
                                     :test 'string=)))
      (if (not todo-type)
          ;; NOTE(nox): No todo keyword
          (let* ((timestamp (or scheduled-ts deadline-ts))
                 (time-to-now (and timestamp (org-time-stamp-to-now timestamp)))
                 first-child search-bound temp-time)

            ;; NOTE(nox): Find the most recent active timestamp
            (when (and (not time-to-now) contents-begin)
              (setq first-child (org-element-map (org-element-contents headline) 'headline #'identity
                                                 nil t 'headline)
                    search-bound (or (and first-child (org-element-property :begin first-child))
                                     (org-element-property :end headline)))
              (goto-char contents-begin)
              (while (re-search-forward org-ts-regexp search-bound t)
                (setq temp-time (org-time-stamp-to-now (match-string 1)))
                (when (or (not time-to-now) (> temp-time time-to-now))
                  (setq time-to-now temp-time))))

            (if (and time-to-now (< time-to-now -60))
                ;; NOTE(nox): This headline without todo keyword has a timestamp that is
                ;; more than two months old.
                (push entry +agenda-archivable-tasks)

              ;; NOTE(nox): Just process the children of this headline without todo keyword
              (unless tickler (setq return (+agenda-process-children headline)))))

        ;; NOTE(nox): Has todo keyword
        (+agenda-set-parent-minimum-status 'stuck)

        (if (eq todo-type 'done)
            ;; NOTE(nox): Archive all tasks that have been done for longer than 2 months
            (when (or (not closed-ts) (< (org-time-stamp-to-now closed-ts) -60))
              (push entry +agenda-archivable-tasks))

          (unless tickler
            (cond
             ;; NOTE(nox): Planned
             ((and (not (string= todo "NEXT")) scheduled-future)
              (setf (+agenda-entry-planned entry) t)
              (if (= +agenda-level 0)
                  (push entry +agenda-planned-tasks)
                (+agenda-set-parent-minimum-status 'planned)
                (setq return entry)))

             ;; NOTE(nox): Hold
             ((or (string= todo "HOLD") (string= todo "WAITING"))
              (push entry +agenda-hold-tasks))

             (t
              ;; NOTE(nox): Process children
              (let* ((+agenda-project-status nil)
                     (children (+agenda-flatten-list (+agenda-process-children headline t)))
                     tail prev)
                (setq project-status +agenda-project-status)

                ;; NOTE(nox): When this project is not planned, we need to remove its
                ;; planned tasks and insert them in the planned list
                ;; IMPORTANT(nox): A project that is stuck doesn't have any planned children
                ;; so, for this check, not planned ≡ next
                (when (eq project-status 'next)
                  (setq tail children)
                  (while tail
                    (if (or (+agenda-entry-planned (car tail))
                            (eq (+agenda-entry-project-status (car tail)) 'planned))
                        (progn
                          (push (car tail) +agenda-planned-tasks)
                          (if prev
                              (setcdr prev (cdr tail))
                            (setq children (cdr tail))))
                      (setq prev tail))
                    (setq tail (cdr tail))))

                (setf (+agenda-entry-project-status entry) project-status
                      (+agenda-entry-children entry) children))

              ;; NOTE(nox): Update parent project status
              (unless (or (= +agenda-level 0) (eq +agenda-project-status 'next))
                (if project-status
                    (when (memq project-status '(next planned)) (setq +agenda-project-status project-status))
                  (when (or (string= todo "NEXT") scheduled-past-or-now)
                    (setq +agenda-project-status 'next))))

              (if project-status
                  (if (and (eq project-status 'planned) (= +agenda-level 0))
                      (push entry +agenda-planned-tasks)
                    (setq return entry))

                (if (= +agenda-level 0)
                    (unless (+agenda-filter-priorities entry)
                      (when (or (not has-scheduling) (and (string= todo "NEXT")
                                                          scheduled-future))
                        (push entry +agenda-isolated-tasks)))

                  (when (or (string= todo "NEXT") scheduled-past-or-now) (setq return entry)))))))))
      return)))

(defun +agenda-tasks (&optional _)
  (catch 'exit
    (let ((files (org-agenda-files nil 'ifmode))
          +agenda-projects +agenda-isolated-tasks +agenda-high-priority
          +agenda-low-priority +agenda-planned-tasks +agenda-hold-tasks
          +agenda-archivable-tasks
          org-todo-regexp org-not-done-regexp org-complex-heading-regexp org-done-keywords
          org-done-keywords-for-agenda file buffer ast)
      (while (setq file (pop files))
        (org-check-agenda-file file)
        (setq buffer (if (file-exists-p file)
                         (org-get-agenda-file-buffer file)
                       (error "No such file %s" file)))

        (unless org-todo-regexp
          (dolist (variable '(org-todo-regexp org-not-done-regexp org-complex-heading-regexp
                                              org-done-keywords org-done-keywords-for-agenda))
            (set variable (buffer-local-value variable buffer))))

        (with-current-buffer buffer
          (org-with-wide-buffer
           (unless (derived-mode-p 'org-mode) (error "Agenda file %s is not in Org mode" file))
           (setq ast (org-element-parse-buffer 'headline))
           (let ((+agenda-level 0)
                 +agenda-parent-tags)
             (setq +agenda-projects
                   (append
                    (+agenda-flatten-list
                     (org-element-map ast 'headline #'+agenda-tasks-process-headline nil nil 'headline))
                    +agenda-projects))))))

      (let ((inhibit-read-only t))
	    (goto-char (point-max))
        (+agenda-render-block (nreverse +agenda-high-priority)    "Alta prioridade")
        (+agenda-render-block +agenda-projects                    "Projetos" #'+agenda-project-printer)
        (+agenda-render-block (nreverse +agenda-isolated-tasks)   "Tarefas isoladas")
        (+agenda-render-block (nreverse +agenda-low-priority)     "Baixa prioridade")
        (+agenda-render-block (nreverse +agenda-archivable-tasks) "Tarefas a arquivar")
        (+agenda-render-block (nreverse +agenda-planned-tasks)    "Tarefas planeadas")
        (+agenda-render-block (nreverse +agenda-hold-tasks)       "Tarefas em espera")))))

;;; Private information

(defvar +agenda-show-private t
  "If non-nil, show sensitive information on the agenda.")

(defun +agenda/toggle-private ()
  (interactive)
  (setq +agenda-show-private (not +agenda-show-private))
  (when  (equal major-mode 'org-agenda-mode) (org-agenda-redo))
  (message "Private tasks: %s" (if +agenda-show-private "Shown" "Hidden")))



;;; Compatibility with their functions

(defun +agenda*change-all-lines-fixface (newhead hdmarker &optional fixface just-this)
  (when (org-get-at-bol 'nox-custom-agenda)
    (let* ((inhibit-read-only t)
           (bol (point-at-bol))
           (eol (point-at-eol))
           (position (next-single-property-change bol 'nox-face nil eol)))
	  (add-text-properties bol eol `(face ,(and position (get-text-property position 'nox-face)))))))
(advice-add 'org-agenda-change-all-lines :before '+agenda*change-all-lines-fixface)
