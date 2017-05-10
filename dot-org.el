(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(add-hook 'org-mode-hook
          (lambda () (visual-line-mode 1)))

(setq org-export-backends '(html latex))
(setq org-modules '(org-habit))

(setq org-archive-save-context-info nil)
(setq org-habit-show-habits-only-for-today nil)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-startup-indented t)
(setq org-use-fast-todo-selection t)
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-outline-path-complete-in-steps nil)
(setq org-drawers '(("PROPERTIES" "LOGBOOK")))
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)"
                  "|" "DONE(d!)" "CANCELED(c@)")))

(cond ((eq system-type 'gnu/linux)
       (setq org-directory "~/Sync/org")
       (setq org-agenda-files '("~/Sync/org")))
      ((eq system-type 'windows-nt)
       (setq org-directory "C:/Users/nga/Sync/org")
       (setq org-agenda-files '("C:/Users/nga/Sync/org"))))

(setq org-capture-templates
      '(("x" "New note" entry
         (file "inbox.org")
         "* %?\n%U\n\n")
        ("t" "New task" entry
         (file+headline "todo.org" "Tasks")
         "* TODO %?\n%U\n\n")))

(setq org-agenda-custom-commands
      '(;; Calendar
        ("c" "Calendar"
         ((agenda "" ((org-agenda-ndays 7)
                      (org-agenda-time-grid nil)
                      (org-agenda-entry-types '(:timestamp :sexp :deadline :scheduled))
                      (org-agenda-repeating-timestamp-show-all t)
                      (org-agenda-start-on-weekday nil)))
          (todo "WAITING"
                ((org-agenda-overriding-header "\nWaiting: "))))
         ;; options for entire block calendar
         ((org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)))
        ("o" "Office" tags-todo "office")
        ("j" "H/A/L" tags-todo "a")
        ))

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
