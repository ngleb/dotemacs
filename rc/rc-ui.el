;; UI settings
(provide 'rc-ui)

;; Window size and position
;;
(setq default-frame-alist
      '(
        (width . 100)
        (height . 35)
        (top . 200)
        (left . 400)
        ))

;; Font setup
(add-to-list 'default-frame-alist
             '(font . "Hack 11"))

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
;; (setq column-number-mode t)
;; (setq-default indicate-empty-lines t)
;; (setq-default show-trailing-whitespace t)

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

;; EOF
