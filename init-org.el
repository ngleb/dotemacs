
(eval-and-compile
  (require 'cl-lib))

(eval-when-compile
  (require 'cl))

(require 'org)
(require 'org-agenda)
(require 'org-protocol)
(require 'org-capture)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))

(setq org-export-backends '(html latex ascii))
(setq org-modules '(org-habit org-protocol))

(bind-keys
 ("C-c l" . #'org-store-link)
 ("C-c a" . #'org-agenda)
 ("C-c c" . #'org-capture)
 ("C-c b" . #'org-switchb)
 ("<f12>" . (lambda () (interactive) (gn/open-agenda " " nil))))

(use-package org-habit
  :config
  (setq org-habit-graph-column 55
        org-habit-preceding-days 30
        org-habit-following-days 1
        org-habit-today-glyph ?@)
  (setq org-habit-show-habits-only-for-today nil))

(use-package ox-clip
  :commands ox-clip-formatted-copy)

(setq org-startup-indented t)
(setq org-log-done 'time)
(setq org-log-reschedule 'time)
(setq org-log-into-drawer t)
(setq org-use-fast-todo-selection t)
(setq org-archive-save-context-info nil)
(setq org-capture-bookmark nil)
(setq org-drawers '(("PROPERTIES" "LOGBOOK")))
(setq org-fast-tag-selection-single-key 't)

(defvar gn-org-agenda-file)
(defvar gn-org-someday-file)
(setq org-directory (expand-file-name "Sync/org/" gn-base-dir))
(setq gn-org-agenda-file (expand-file-name "todo.org" org-directory))
(setq gn-org-someday-file (expand-file-name "someday.org" org-directory))
(setq org-default-notes-file (expand-file-name "refile.org" org-directory)
      org-agenda-files (list org-default-notes-file gn-org-agenda-file))

;; Refile setup
(setq org-refile-targets '((nil :maxlevel . 9)
                           (gn-org-agenda-file :maxlevel . 2)
                           (gn-org-someday-file :maxlevel . 1)))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-outline-path-complete-in-steps nil)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)"  "|" "DONE(d!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)"))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-agenda-tags-column -110)
(setq org-agenda-span 'day)
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-todo-ignore-scheduled 'all)
(setq org-agenda-todo-ignore-deadlines 'all)
(setq org-agenda-todo-ignore-with-date 'all)
(setq org-agenda-todo-ignore-timestamp 'all)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-todo-list-sublevels nil)
(setq org-agenda-block-separator "")
(setq org-agenda-prefix-format '((agenda . "  %?-12t% s")))
(setq org-agenda-compact-blocks t)
(setq org-enforce-todo-dependencies t)

(defun transform-square-brackets-to-round-ones (string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged in STRING-TO-TRANSFORM."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

(setq org-capture-templates
      '(("x" "note" entry (file "refile.org")
         "* %?\nAdded on: %U\n")
        ("t" "todo" entry (file "refile.org")
         "* TODO %?\nAdded on: %U\n")
        ("n" "todo clocked" entry (file "refile.org")
         "* TODO %?\nAdded on: %U\n" :clock-in t :clock-resume t)
        ("j" "Journal" entry (file+olp+datetree "diary.org")
         "* %?\nAdded on: %U\n" :clock-in t :clock-resume t)
        ("p" "org-protocol" entry (file "refile.org")
         "* Review [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\nAdded on: %U\n" :immediate-finish t)
        ("w" "Protocol selected" entry (file "refile.org")
        "* %^{Title}\nSource: [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?")
        ("h" "Habit" entry (file "refile.org")
         "* NEXT %?\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

(setq org-agenda-custom-commands
      '(("h" "Habits" tags-todo "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-strategy
           '(todo-state-down effort-up category-keep))))
        ("n" "Agenda"
         ((agenda "" ((org-agenda-files (list org-default-notes-file gn-org-agenda-file))
                      (org-agenda-span 3)))
          (+agenda-inbox nil ((org-agenda-files (list org-default-notes-file))))
          (+agenda-tasks nil ((org-agenda-files (list gn-org-agenda-file))))))
        (" " "Agenda"
         ((agenda ""
                  ((org-agenda-time-grid nil)
                   (org-agenda-entry-types '(:timestamp :sexp :deadline :scheduled))))
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)
                 (org-agenda-prefix-format "  %b")
                 (org-agenda-remove-tags t)))
          (tags-todo "-MAYBE-CANCELLED-REFILE/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-MAYBE-CANCELLED-REFILE/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-skip-function 'bh/skip-non-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANCELLED-REFILE/!NEXT"
                     ((org-agenda-overriding-header "Project Next Tasks")
                      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled 'all)
                      (org-agenda-todo-ignore-deadlines 'all)
                      (org-agenda-todo-ignore-with-date 'all)
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
          (tags-todo "-MAYBE-REFILE-CANCELLED-WAITING-HOLD/!"
                     ((org-agenda-overriding-header "Project Tasks")
                      (org-agenda-skip-function 'bh/skip-non-project-tasks)
                      (org-agenda-todo-ignore-scheduled 'all)
                      (org-agenda-todo-ignore-deadlines 'all)
                      (org-agenda-todo-ignore-with-date 'all)
                      (org-agenda-sorting-strategy
                       '(tag-up effort-up))))
          (tags-todo "-MAYBE-REFILE-CANCELLED-WAITING/!"
                     ((org-agenda-overriding-header "Tasks")
                      (org-agenda-skip-function 'bh/skip-project-tasks)
                      (org-agenda-todo-ignore-scheduled 'all)
                      (org-agenda-todo-ignore-deadlines 'all)
                      (org-agenda-todo-ignore-with-date 'all)
                      (org-agenda-sorting-strategy
                       '(tag-up effort-up))))
          (tags-todo "-MAYBE-CANCELLED+WAITING|HOLD/!"
                     ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                      (org-agenda-skip-function 'bh/skip-non-tasks)
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-todo-ignore-scheduled 'all)
                      (org-agenda-todo-ignore-deadlines 'all)))
          (tags "-REFILE-MAYBE/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                 (org-tags-match-list-sublevels nil)))
          )
         ;; options for entire block calendar
         ((org-agenda-remove-tags nil)
          (org-agenda-prefix-format "  %?-12t% s")))))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project."
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects."
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-non-tasks2 ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        next-headline)
       ((and (bh/is-project-subtree-p) (not (member (org-get-todo-state) (list "NEXT"))))
        next-headline)
       (t
        nil)))))

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving."
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))


;;; Narrowing to a subtree with `bh/org-todo'

(global-set-key (kbd "<f5>") 'bh/org-todo)

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(global-set-key (kbd "<S-f5>") 'bh/widen)

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
          'append)

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (bh/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
          'append)

(defun bh/narrow-to-org-subtree ()
  "Narrow to org subtree."
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (bh/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (bh/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (org-get-at-bol 'org-hd-marker)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-up-one-org-level))
        (org-agenda-redo))
    (bh/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-to-org-project)
          (save-excursion
            (bh/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (beginning-of-buffer))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

(defvar bh/project-list nil)

(defun bh/view-next-project ()
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
      ; Clear all of the existing markers on the list
      (while bh/project-list
        (set-marker (pop bh/project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

    ; Build a new project marker list
    (unless bh/project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (bh/is-project-p))
                              (bh/is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
        (forward-visible-line 1)))

    ; Pop off the first marker on the list and display
    (setq current-project (pop bh/project-list))
    (when current-project
      (org-with-point-at current-project
        (setq bh/hide-scheduled-and-waiting-next-tasks nil)
        (bh/narrow-to-project))
      ; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (beginning-of-buffer)
      (setq num-projects-left (length bh/project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (beginning-of-buffer)
        (setq bh/hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed.")))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append)

;;; Limiting the agenda to a subtree

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
          'append)

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))
          (org-agenda-redo))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

(setq org-agenda-restriction-lock-highlight-subtree nil)


;;; Use org-capture with separate frame

(defun make-capture-frame (&optional capture-url)
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")
                (width . 100)
                (height . 30)))
  (select-frame-by-name "capture")
  (org-capture nil "x"))

(defadvice org-capture-finalize (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame if it is the capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame if it is the rememeber frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defun gn/open-agenda (&optional arg split)
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive "P")
  (org-agenda nil arg)
  (when (not split)
    (delete-other-windows)))


;;; Entry

(cl-defstruct +agenda-entry todo priority text tags planned low-effort marker project-status children)

(defun +agenda-entry (headline &optional tags)
  (let ((todo-type (org-element-property :todo-type headline))
        (effort (org-element-property :EFFORT headline)))
    (make-+agenda-entry
     :todo (org-element-property :todo-keyword headline)
     :priority (org-element-property :priority headline)
     :text (org-element-property :raw-value headline)
     :tags (or tags (org-element-property :tags headline))
     :low-effort (and effort (eq todo-type 'todo) (< (org-duration-to-minutes effort) 20))
     :marker (org-agenda-new-marker (org-element-property :begin headline)))))


;;; Renderer

(defconst +agenda-projects-not-task-faces '(("NEXT" . '(:inherit org-todo :weight normal))
                                            ("TODO" . '(:inherit org-todo :weight normal))))

(defconst +agenda-projects-task-faces '(("NEXT" . '(:inherit org-todo :weight bold))
                                        ("TODO" . '(:inherit org-todo :weight bold))))

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

(defun +agenda-tag-sort (a b)
  (let ((pa (car (+agenda-entry-tags a)))
        (pb (car (+agenda-entry-tags b))))
    (if (string-lessp pa pb) t)))

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
  (setq list (sort list #'+agenda-priority-sort))
;;  (setq list (sort list #'+agenda-tag-sort))
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

(defun +agenda-tasks-process-headline (headline)
  (let* ((todo (org-element-property :todo-keyword headline))
         (todo-type (org-element-property :todo-type headline))
         (scheduled-ts (org-element-property :raw-value (org-element-property :scheduled headline)))
         (deadline-ts  (org-element-property :raw-value (org-element-property :deadline headline)))
         (closed-ts  (org-element-property :raw-value (org-element-property :closed headline)))
         (has-scheduling (or scheduled-ts deadline-ts))
         (scheduled-future (cond (scheduled-ts (> (org-time-stamp-to-now scheduled-ts) 0))
                                 (deadline-ts  (> (org-time-stamp-to-now deadline-ts)
                                                  (org-get-wdays deadline-ts)))))
         (scheduled-past-or-now (and has-scheduling (not scheduled-future)))
         (effort (org-element-property :EFFORT headline))
         (contents-begin (org-element-property :contents-begin headline))
         (tickler (member "TICKLER" (org-element-property :tags headline)))
         entry project-status return timestamp-pos)

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

        (setq no-timestamp t)
        ;; verify if entry has timestamp
        (let* ((timestamp (or scheduled-ts deadline-ts))
               first-child search-bound)
          (when contents-begin
            (setq first-child (org-element-map (org-element-contents headline) 'headline #'identity
                                               nil t 'headline)
                  search-bound (or (and first-child (org-element-property :begin first-child))
                                   (org-element-property :end headline)))
            (goto-char contents-begin)

            (if (re-search-forward org-ts-regexp search-bound t)
                (setq no-timestamp nil)
              (setq no-timestamp t))))

        ;;(message "\n[TEST] %s" (org-element-property :raw-value headline))
        ;;(message "no-ts %s | ds %s | sc %s" no-timestamp deadline-ts scheduled-ts)
        (setq no-timestamp (or no-timestamp (or scheduled-ts deadline-ts)))
        ;;(message "no-ts %s\n" no-timestamp)

        ;; process entried without timestamps, sheduled or deadlines
        (when no-timestamp

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

                  (when (or (string= todo "NEXT") scheduled-past-or-now) (setq return entry))))))))))
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

(provide 'init-org)

;;; init-org.el ends here
