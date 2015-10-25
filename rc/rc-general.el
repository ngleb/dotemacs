;;; rc-general.el ---
(provide 'rc-general)

;; File for custom setup
;;
(setq custom-file "~/.emacs.d/custom.el")

;; MULE setup
;;
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq default-input-method "russian-computer")

;; Tab & indent setup
;;
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

;; Keyboard bindings
;;
(global-set-key (kbd "<f10>") 'save-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; dired+ setup
;;
(setq diredp-hide-details-initially-flag nil)
(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1) ; use the same buffer without creating others

;; backup settings
;;
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; Helm settings
;;
(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(helm-mode 1)
(helm-autoresize-mode t)

;;; rc-general.el ends here
