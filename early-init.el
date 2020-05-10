;;; early-init.el --- Load the early before-init configuration -*- lexical-binding: t -*-
;;; Commentary:

;; early-init.el

;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)



(provide 'early-init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; early-init.el ends here
