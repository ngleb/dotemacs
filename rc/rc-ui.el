;;; rc-ui.el ---

;; UI settings
(provide 'rc-ui)

(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Frame size, position, font
;;
(setq default-frame-alist
      '(
        (width . 120)
        (height . 40)
        (top . 200)
        (left . 400)
        (font . "Hack 11")
        ))

;; Window elements
;;
(blink-cursor-mode -1)
(show-paren-mode 1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Other general options
;;
;; (setq visible-bell t)
(setq column-number-mode t)
;; (setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)

;; Theme settings
;;
(setq x-underline-at-descent-line t)
(setq monokai-use-variable-pitch nil)
(setq monokai-height-minus-1 1.0)
(setq monokai-height-plus-1 1.0)
(setq monokai-height-plus-2 1.0)
(setq monokai-height-plus-3 1.0)
(setq monokai-height-plus-4 1.0)
(load-theme 'monokai t)

;; Powerline settins
;;
(require 'powerline)
(powerline-default-theme)

;; smooth-scrolling
;;
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

;;; rc-ui.el ends here
