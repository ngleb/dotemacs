(provide 'rc-files)

(setq diredp-hide-details-initially-flag nil)
(require 'dired+)

;; backup settings
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

; end of rc-files.el file
