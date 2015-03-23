(setq custom-file "~/.emacs.d/custom.el")

; package setup
(setq byte-compile-warnings nil)
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(setq default-frame-alist
	  '(
		(width . 120)
		(height . 40)
		(top . 200)
		(left . 500)
		))

;; MULE setup
(set-language-environment 'UTF-8)
(setq default-input-method "russian-computer")

(require 'ido)
(ido-mode 1)

(setq enable-local-variables :safe)
(setq inhibit-startup-message t)

;; Tab & indent setup
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

(fset 'yes-or-no-p 'y-or-n-p)
(setq delete-old-versions nil)

;(setq split-width-threshold nil)
(blink-cursor-mode -1)
(show-paren-mode 1)
(savehist-mode 1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(prefer-coding-system 'utf-8)

(add-to-list 'default-frame-alist
			 '(font . "Consolas 10"))

(setq visible-bell t)
(setq show-paren-delay 0)
(setq column-number-mode t)
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(global-set-key (kbd "<f10>") 'save-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

; org-mode settings
(add-hook 'org-mode-hook 'turn-on-font-lock)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-todo-keywords
	  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
		(sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(o)" "|" "CANCELLED(c@/!)")))
(setq org-default-notes-file (expand-file-name "~/my/org/todo.org"))
(setq org-capture-templates
	  '(("r" "Todo" entry (file+headline "~/my/org/inbox.org" "Inbox")
		 "* TODO %?\n %i\n %a")
		("j" "Journal" entry (file+datetree "~/my/org/journal.org")
		 "* %?\n entered on %U\n  %i\n  %a")))
(global-set-key (kbd "C-c r")
				(lambda () (interactive) (org-capture nil "r")))
(global-set-key (kbd "C-c j")
				(lambda () (interactive) (org-capture nil "j")))

; backup settings
(setq make-backup-files t)
;(setq version-control t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq solarized-scale-org-headlines nil)
(setq solarized-use-variable-pitch nil)
(setq x-underline-at-descent-line t)
;(setq solarized-height-minus-1 1)
;(setq solarized-height-plus-1 1)
;(setq solarized-height-plus-2 1)
;(setq solarized-height-plus-3 1)
;(setq solarized-height-plus-4 1)
(load-theme 'solarized-dark t)
