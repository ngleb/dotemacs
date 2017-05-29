(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(add-hook 'org-mode-hook
          (lambda () (visual-line-mode 1)))

(setq org-export-backends '(html latex))
(setq org-modules '(org-habit org-protocol))

(require 'org-protocol)

(setq org-archive-save-context-info nil)
(setq org-habit-show-habits-only-for-today nil)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-startup-indented t)
(setq org-use-fast-todo-selection t)

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

(cond ((eq system-type 'gnu/linux)
       (setq org-directory "~/Sync/org")
       (setq org-agenda-files '("~/Sync/org")))
      ((eq system-type 'windows-nt)
       (setq org-directory "C:/Users/nga/Sync/org")
       (setq org-agenda-files '("C:/Users/nga/Sync/org"))))

(setq org-capture-templates
      '(("x" "note" entry (file "refile.org")
         "* %?\n%U\n")
        ("t" "todo" entry (file "refile.org")
         "* TODO %?\n%U\n")
        ("w" "org-protocol" entry (file "refile.org")
         "* TODO Review [[%:link][%:description]]\n%U\n" :immediate-finish t)))

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-tags-column -102)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

(setq org-agenda-custom-commands
      '(;; Calendar
        (" " "Agenda"
         ((agenda ""
                  ((org-agenda-ndays 7)
                   (org-agenda-time-grid nil)
                   (org-agenda-entry-types '(:timestamp :sexp :deadline :scheduled))
                   (org-agenda-repeating-timestamp-show-all t)
                   (org-agenda-start-on-weekday 1)))
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting")))
          (tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function 'bh/skip-non-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                     ((org-agenda-overriding-header "Tasks")
                      (org-agenda-skip-function 'bh/skip-non-tasks2)
                      (org-agenda-todo-ignore-scheduled 'all)
                      (org-agenda-todo-ignore-deadlines 'all)
                      (org-agenda-todo-ignore-with-date 'all)
                      (org-agenda-sorting-strategy
                       '(tag-up effort-up))))
          )
         ;; options for entire block calendar
         ((org-agenda-remove-tags nil)))))


(defun gn/open-agenda (&optional arg split)
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive "P")
  (org-agenda nil arg)
  (when (not split)
    (delete-other-windows)))

(defun gn/open-agenda-a ()
  (interactive)
  (gn/open-agenda "a" nil))

(defun gn/open-agenda-c ()
  (interactive)
  (gn/open-agenda "c" nil))

(defun gn/org-capture-new-note ()
  (interactive)
  (org-capture nil "x"))

(defun gn/org-capture-task ()
  (interactive)
  (org-capture nil "t"))

(bind-key "C-c l" #'org-store-link)
(bind-key "C-c a" #'org-agenda)
(bind-key "C-c c" #'org-capture)
(bind-key "C-c b" #'org-iswitchb)
(bind-key "C-c x" #'gn/org-capture-new-note)
(bind-key "C-c t" #'gn/org-capture-task)
(bind-key "<f10>" #'gn/open-agenda)
(bind-key "<f11>" #'gn/open-agenda-c)
(bind-key "<f12>" #'gn/open-agenda-a)

(provide 'dot-org)

;; dot-org.el ends here
