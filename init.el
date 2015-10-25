;;; init.el ---

;; package.el settins
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(add-to-list 'load-path "~/.emacs.d/rc")

(require 'rc-general)
(require 'rc-ui)
(require 'rc-org)
(require 'rc-personal)

;;; init.el ends here
