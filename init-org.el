;;; init-org.el --- Load org-mode configuration -*- lexical-binding: t -*-
;;; Commentary:

;; org-mode config

;;; Code:

(require 'org)
(use-package org-agenda)
(use-package org-capture)
(use-package org-protocol)
(use-package org-checklist)
(use-package org-tempo)

(add-hook 'org-mode-hook 'visual-line-mode)
(add-to-list 'org-modules 'org-habit)
(add-to-list 'org-modules 'org-protocol)
(add-to-list 'org-modules 'org-checklist)
(setq org-export-backends '(html latex ascii))

;; List of additional LaTeX packages
(add-to-list 'org-latex-packages-alist '("" "cmap" t))
(add-to-list 'org-latex-packages-alist '("english,russian" "babel" t))

(bind-keys
 ("C-c l" . org-store-link)
 ("C-c a" . org-agenda)
 ("C-c c" . org-capture)
 ("C-c b" . org-switchb)
 ("<f12>" . (lambda () (interactive) (gn/open-agenda "g" nil)))
 :map org-agenda-mode-map
 ("<f10>" . org-agenda-switch-to-narrowed-subtree))

(setq org-directory (expand-file-name "Sync/org/" gn-base-dir))
(setq org-default-notes-file (expand-file-name "refile.org" org-directory))
(setq gn-org-agenda-file (expand-file-name "todo.org" org-directory))
(setq gn-org-someday-file (expand-file-name "someday.org" org-directory))
(setq org-agenda-files (list gn-org-agenda-file))

(setq gn-org-mobile-file (expand-file-name "Nextcloud/org-mobile/mobile.org" gn-base-dir))
(setq gn-org-purchases (expand-file-name "Nextcloud/org-mobile/purchases.org" gn-base-dir))

(setq org-startup-indented t)
(setq org-archive-save-context-info nil)
(setq org-capture-bookmark nil)
(setq org-fast-tag-selection-single-key 't)
(setq org-tags-column -90)
(setq org-catch-invisible-edits 'show)
(setq org-export-coding-system 'utf-8)
(setq org-tag-alist '((:startgroup . nil)
                      ("home" . ?h)
                      ("office" . ?o)
                      ("a" . ?a)
                      ("anywhere" . ?w)
                      (:newline . nil)
                      ("laptop" . ?l)
                      ("errands" . ?e)
                      ("reading" . ?r)
                      ("video" . ?v)
                      ("calls" . ?c)
                      (:endgroup . nil)))

(setq org-enable-priority-commands nil)

;;; org babel

(setq org-confirm-babel-evaluate nil)


;;; org refile

(setq org-refile-targets '((nil :maxlevel . 9)
                           (gn-org-agenda-file :maxlevel . 2)
                           (gn-org-someday-file :maxlevel . 1)
                           (org-default-notes-file :maxlevel . 1)
                           (gn-org-purchases :maxlevel . 2)))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-cache nil)

;;; todo

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
      org-use-fast-todo-selection t)

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" . "turquoise"))))


;;; org clock

(setq org-clock-persist t)
(setq org-clock-in-resume t)
(org-clock-persistence-insinuate)

(setq org-log-done 'time)
(setq org-log-reschedule 'time)
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)


;;; org capture

(defun transform-square-brackets-to-round-ones (string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged in STRING-TO-TRANSFORM."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

(setq org-capture-templates
      '(("x" "Note" entry (file "")
         "* %?" :clock-resume t :empty-lines 0)
        ("t" "Task" entry (file "")
         "* NEXT %?" :clock-resume t :empty-lines 0)
        ("j" "Journal" entry (file+olp+datetree "")
         "* %?\n")
        ("p" "Link" entry (file "")
         "* Review [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n" :immediate-finish t)
        ("s" "Link with text" entry (file "")
        "* %^{Title}\nSource: [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?")
        ("h" "Habit" entry (file "")
         "* NEXT %?\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))


;;; org agenda

(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


(let ((active-project-match "-INBOX/PROJECT"))

  (setq org-stuck-projects
        `(,active-project-match ("NEXT")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky nil
        org-agenda-start-on-weekday nil
        org-agenda-span 5
        org-agenda-start-day "-2d"
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo tag-up category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'only-window
        org-agenda-custom-commands
        `(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("g" "GTD"
           ((agenda ""
                    ((org-agenda-skip-timestamp-if-done t)
                     (org-agenda-skip-deadline-if-done t)
                     (org-agenda-skip-scheduled-if-done t)
                     (org-deadline-warning-days 7)))
            (tags "INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-files (list org-default-notes-file gn-org-mobile-file))))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'all)
                        (org-agenda-todo-ignore-with-date 'all)
                        (org-agenda-todo-ignore-timestamp 'all)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(todo-state-down tag-up effort-up category-keep))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'all)
                        (org-agenda-todo-ignore-with-date 'all)
                        (org-agenda-todo-ignore-timestamp 'all)
                        (org-agenda-sorting-strategy '(tag-up))
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(tag-up category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        ;(org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/DELEGATED"
                       ((org-agenda-overriding-header "Delegated")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "On Hold")
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags "LEVEL=1+TODO=\"CANCELLED\"|LEVEL=1+TODO=\"DONE\""
                  ((org-agenda-overriding-header "Finished tasks")
                   (org-tags-match-list-sublevels t)))
            ;; (tags-todo "-NEXT"
            ;;            ((org-agenda-overriding-header "All other TODOs")
            ;;             (org-match-list-sublevels t)))
            )))))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)


;;; org habit

(setq org-habit-graph-column 55
      org-habit-preceding-days 30
      org-habit-following-days 1
      org-habit-today-glyph ?@
      org-habit-show-habits-only-for-today t
	  org-habit-show-done-always-green t)

(use-package ox-clip)
(use-package ox-pandoc)
(use-package ox-beamer)


(defun org-agenda-switch-to-narrowed-subtree ()
  (interactive)
  (org-agenda-switch-to)
  (org-narrow-to-subtree))


(defun gn/open-agenda (&optional arg split)
  "Visit the org agenda `ARG', in the current window or a `SPLIT'."
  (interactive "P")
  (org-agenda nil arg)
  (when (not split)
    (delete-other-windows)))


;;; Use org-capture with separate frame
;;; command to eval this code: emacsclientw.exe -ne "(make-capture-frame)"
(defun make-capture-frame (&optional capture-url)
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")
                (width . 100)
                (height . 30)
                (org-capture-frame . t)))
  (select-frame-by-name "capture")
  (org-capture))

(advice-add
 'org-switch-to-buffer-other-window :after
 (lambda (&rest _) (when (frame-parameter nil 'org-capture-frame) (delete-other-windows))))

(advice-add
 'org-capture :around
 (lambda (capture-function &rest args)
   (condition-case nil (apply capture-function args)
     (error (when (frame-parameter nil 'org-capture-frame)
              (delete-frame))))))

(add-hook
 'org-capture-after-finalize-hook
 (lambda (&rest _)
   (when (and (frame-parameter nil 'org-capture-frame) (not org-capture-is-refiling))
     (org-save-all-org-buffers)
     (delete-frame))))

(advice-add
 'org-capture-refile :after
 (lambda (&rest _)
   (when (frame-parameter nil 'org-capture-frame)
     (org-save-all-org-buffers)
     (delete-frame))))



(provide 'init-org)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-org.el ends here
