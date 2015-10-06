; UI settings
(provide 'rc-ui)

(setq default-frame-alist
      '(
        (width . 100)
        (height . 35)
        (top . 200)
        (left . 400)
        ))

(blink-cursor-mode -1)
(show-paren-mode 1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

; font setup
(add-to-list 'default-frame-alist
             '(font . "Hack 11"))

(setq x-underline-at-descent-line t)
(setq monokai-use-variable-pitch nil)
(setq monokai-height-minus-1 1.0)
(setq monokai-height-plus-1 1.0)
(setq monokai-height-plus-2 1.0)
(setq monokai-height-plus-3 1.0)
(setq monokai-height-plus-4 1.0)

(load-theme 'monokai t)

; EOF
