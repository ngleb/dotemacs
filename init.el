(setq custom-file "~/.emacs.d/custom.el")

; package setup
(setq byte-compile-warnings nil)
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(add-to-list 'load-path "~/.emacs.d/rc")

(require 'rc-ui)
(require 'rc-org)
(require 'rc-personal)
(require 'rc-files)

;; MULE setup
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq default-input-method "russian-computer")

; Tab & indent setup
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(helm-mode 1)

(setq visible-bell t)
(setq show-paren-delay 0)
(setq column-number-mode t)
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)

;; keyboard setup
(global-set-key (kbd "<f10>") 'save-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; end of init.el file
