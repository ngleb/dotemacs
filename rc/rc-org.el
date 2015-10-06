(provide 'rc-org)

;; org-mode settings
(add-hook 'org-mode-hook 'turn-on-font-lock)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-directory "~/OrgMode")
(find-file (concat org-directory "/main.org"))

;; agenda
(setq org-agenda-files (list org-directory))

(setq org-refile-use-outline-path t)

;; capture
(setq org-default-notes-file (concat org-directory "/inbox.org"))

(setq org-capture-templates
      '(("x" "New inbound entry" entry (file (concat org-directory "/inbox.org"))
         "* %?\n  %U\n  %i"
         :empty-lines 1)
        ))

(define-key global-map "\C-cx"
  (lambda () (interactive) (org-capture nil "x")))

; end of rc-org.el file
