;; File for custom setup
;;
(setq custom-file "~/.emacs.d/custom.el")

;; package.el settins
;;
(setq byte-compile-warnings nil)
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Personal config
;;
(add-to-list 'load-path "~/.emacs.d/rc")

(require 'rc-ui)
(require 'rc-org)
(require 'rc-personal)
(require 'rc-files)

;; MULE setup
;;
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq default-input-method "russian-computer")

;; Tab & indent setup
;;
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Helm settings
;;
(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(helm-mode 1)
(helm-autoresize-mode t)

;; smooth-scrolling
;;
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

;; General keyboard bindings
;;
(global-set-key (kbd "<f10>") 'save-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; end of init.el file
