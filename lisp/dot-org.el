(require 'cl)
(require 'org)
(require 'org-protocol)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(add-hook 'org-mode-hook (lambda () (visual-line-mode 1)))
(add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))

(setq org-export-backends '(html latex ascii))
(setq org-modules '(org-habit org-protocol))

(setq org-archive-save-context-info nil)
(setq org-habit-show-habits-only-for-today nil)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-startup-indented t)
(setq org-use-fast-todo-selection t)
(setq org-capture-bookmark nil)

;; Refile setup
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-outline-path-complete-in-steps nil)

(setq org-drawers '(("PROPERTIES" "LOGBOOK")))
(setq org-fast-tag-selection-single-key (quote t))

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

(setq org-directory
      (expand-file-name "Sync/org/" gn/default-dir))
(setq org-agenda-files
      (list (expand-file-name "Sync/org/" gn/default-dir)))

(setq org-capture-templates
      '(("x" "note" entry (file "refile.org")
         "* %?\nAdded on: %U\n")
        ("t" "todo" entry (file "refile.org")
         "* TODO %?\nAdded on: %U\n")
        ("n" "todo clocked" entry (file "refile.org")
         "* TODO %?\nAdded on: %U\n" :clock-in t :clock-resume t)
        ("j" "Journal" entry (file+olp+datetree "diary.org")
         "* %?\nAdded on: %U\n" :clock-in t :clock-resume t)
        ("w" "org-protocol" entry (file "refile.org")
         "* TODO Review [[%:link][%:description]]\nAdded on: %U\n" :immediate-finish t)
        ("h" "Habit" entry (file "refile.org")
         "* NEXT %?\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-tags-column -102)
(setq org-agenda-span 'day)

(setq org-enforce-todo-dependencies t)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

(setq org-agenda-custom-commands
      '(("h" "Habits" tags-todo "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-agenda-sorting-strategy
           '(todo-state-down effort-up category-keep))))
        (" " "Agenda"
         ((agenda ""
                  ((org-agenda-time-grid nil)
                   (org-agenda-entry-types '(:timestamp :sexp :deadline :scheduled))))
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
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
         ((org-agenda-remove-tags nil)))))

(defun gn/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "office")
         t)
        ((string= tag "hold")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'gn/org-auto-exclude-function)

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
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
  "Move point to the parent (project) task if any"
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
  "Any task with a todo keyword and no subtask"
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
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
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
  "Skip trees that are not projects"
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
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
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
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
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

(defun make-capture-frame (&optional capture-url)
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")
                (width . 120)
                (height . 15)))
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

(bind-key "C-c l" #'org-store-link)
(bind-key "C-c a" #'org-agenda)
(bind-key "C-c c" #'org-capture)
(bind-key "C-c b" #'org-iswitchb)

(bind-key "<f12>" (lambda () (interactive) (gn/open-agenda " " nil)))
(bind-key "C-c x" (lambda () (interactive) (org-capture nil "x")))
(bind-key "C-c t" (lambda () (interactive) (org-capture nil "t")))

(provide 'dot-org)

;; dot-org.el ends here
