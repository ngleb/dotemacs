;;; rc-general.el ---
(provide 'rc-general)

;; File for custom setup
;;
(setq custom-file "~/.emacs.d/custom.el")

;; Helm settings
;;
(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(helm-mode 1)
(helm-autoresize-mode t)

;; General keyboard bindings
;;
(global-set-key (kbd "<f10>") 'save-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c e") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;;; rc-general.el ends here
