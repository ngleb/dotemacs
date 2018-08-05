;;; init.el

(defvar before-user-init-time (current-time)
  "Value of `current-time' when Emacs begins loading `user-init-file'.")
(message "Loading Emacs...done (%.3fs)"
         (float-time (time-subtract before-user-init-time
                                    before-init-time)))

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

(setq package-pinned-packages
      '((avy . "melpa")
        (bind-key . "melpa")
        (company . "melpa-stable")
        (counsel . "melpa-stable")
        (dash . "melpa-stable")
        (deft . "melpa")
        (dired+ . "melpa")
        (docker-compose-mode . "melpa")
        (dockerfile-mode . "melpa")
        (flycheck . "melpa-stable")
        (flycheck-ledger . "melpa")
        (flyspell-popup . "melpa-stable")
        (hydra . "melpa")
        (ivy . "melpa-stable")
        (langtool . "melpa")
        (ledger-mode . "melpa")
        (markdown-mode . "melpa")
        (nlinum . "gnu")
        (olivetti . "melpa")
        (org-plus-contrib . "org")
        (popup . "melpa-stable")
        (smart-mode-line . "melpa")
        (smartparens . "melpa")
        (smex . "melpa")
        (smooth-scrolling . "melpa")
        (swiper . "melpa-stable")
        (use-package . "melpa")
        (w32-browser . "melpa")
        (web-mode . "melpa")
        (zenburn-theme . "melpa")
        (helm . "melpa")
        (helm-core . "melpa")
        (helm-swoop . "melpa")

        (magit . "melpa-stable")
        (magit-popup . "melpa-stable")
        (async . "melpa-stable")
        (git-commit . "melpa-stable")
        (with-editor . "melpa-stable")

        (elpy . "melpa-stable")
        (find-file-in-project . "melpa-stable")
        (highlight-indentation . "melpa-stable")
        (pyvenv . "melpa-stable")))

(package-initialize)
(setq package-contents-refreshed nil)

(mapc (lambda (pinned-package)
        (let ((package (car pinned-package))
              (archive (cdr pinned-package)))
          (unless (package-installed-p package)
            (unless package-contents-refreshed
              (package-refresh-contents)
              (setq package-contents-refreshed t))
            (message "Installing %s from %s" package archive)
            (package-install package))))
      package-pinned-packages)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(defvar gn-base-dir-tmp
  (cond ((eq system-type 'gnu/linux)
         (expand-file-name "~"))
        ((eq system-type 'windows-nt)
         (expand-file-name user-login-name "C:/Users"))))
(setq gn-base-dir (file-name-as-directory gn-base-dir-tmp))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'nlinum-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(setq x-underline-at-descent-line t)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq require-final-newline t)
(setq sentence-end-double-space nil)
(setq scroll-preserve-screen-position 1)
(setq-default truncate-lines t)
(setq-default word-wrap t)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(put 'narrow-to-region 'disabled nil)
(setq x-wait-for-event-timeout nil)

;; remove warning
;; ad-handle-definition: `tramp-read-passwd' got redefined
(setq ad-redefinition-action 'accept)

;; MULE & encoding setup
(setq default-input-method "russian-computer")

;; Stop creating backup and auto-save files
(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #auto-save# files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(global-auto-revert-mode 1)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(bind-key "M-n" (kbd "C-u 1 C-v"))
(bind-key "M-p" (kbd "C-u 1 M-v"))
(bind-key "M-z" #'zap-up-to-char)
(bind-key "M-/" #'hippie-expand)
(bind-key "<f5>" #'toggle-truncate-lines)
(bind-key "C-c e" (lambda () (interactive) (find-file user-init-file)))
(bind-key "C-c C-e" #'pp-eval-last-sexp)
(bind-key [remap eval-expression] #'pp-eval-expression)

(defun fill-sentence ()
  (interactive)
  (save-excursion
    (or (eq (point) (point-max)) (forward-char))
    (forward-sentence -1)
    (indent-relative t)
    (let ((beg (point))
          (ix (string-match "LaTeX" mode-name)))
      (forward-sentence)
      (if (and ix (equal "LaTeX" (substring mode-name ix)))
          (LaTeX-fill-region-as-paragraph beg (point))
        (fill-region-as-paragraph beg (point))))))

;; ediff
;; use existing frame instead of creating a new one
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
;; `d` for using A and B into C
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map ()
  (bind-key "d" #'ediff-copy-both-to-C ediff-mode-map))
;;  (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(defconst display-name
  (pcase (display-pixel-height)
    (`768 'lenovo)
    (`1200 'lenovo-m)
    (`1080 'office)))

(defconst emacs-min-top 20)

(defconst emacs-min-left
  (pcase display-name
    (`lenovo 100)
    (`lenovo-m 210)
    (`office 100)))

(defconst emacs-min-height
  (pcase display-name
    (`lenovo 40)
    (`lenovo-m 53)
    (`office 40)))

(defconst emacs-min-width
  (pcase display-name
    (`lenovo 140)
    (`lenovo-m 164)
    (`office 160)))

(defun emacs-min ()
  (interactive)
  (cl-flet ((set-param (p v) (set-frame-parameter (selected-frame) p v)))
    (set-param 'fullscreen nil)
    (set-param 'vertical-scroll-bars nil)
    (set-param 'horizontal-scroll-bars nil))
  (set-frame-position (selected-frame) emacs-min-left emacs-min-top)
  (set-frame-size (selected-frame) emacs-min-width emacs-min-height))

(defun emacs-max ()
  (cl-flet ((set-param (p v) (set-frame-parameter (selected-frame) p v)))
    (set-param 'fullscreen 'maximized)
    (set-param 'vertical-scroll-bars nil)
    (set-param 'horizontal-scroll-bars nil)))

(defun emacs-toggle-size ()
  (interactive)
  (if (alist-get 'fullscreen (frame-parameters))
      (emacs-min)
    (emacs-max)))

(add-hook 'emacs-startup-hook #'emacs-min t)
(bind-key "C-<f12>" #'emacs-toggle-size)

(use-package isearch
  :bind (:map isearch-mode-map
              ("C-c" . isearch-toggle-case-fold)
              ("C-t" . isearch-toggle-regexp)
              ("C-^" . isearch-edit-string)
              ("C-i" . isearch-complete)))

(use-package avy
  :bind* ("C-." . avy-goto-char-timer)
  :config
  (avy-setup-default))

(use-package elisp-mode
  :config
  (use-package eldoc
    :config
    (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)))

(use-package man
  :config
  (bind-key "j" (kbd "C-u 1 C-v") Man-mode-map)
  (bind-key "k" (kbd "C-u 1 M-v") Man-mode-map))

(use-package ivy
  :bind ("C-c C-r" . ivy-resume)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  (ivy-do-completion-in-region nil)
  ;; :config
  ;; (ivy-mode 1)
  )

(use-package counsel
  :after ivy)
  ;; :bind (("M-x" . counsel-M-x)
  ;;        ("C-x C-f" . counsel-find-file)
  ;;        ("C-c j" . counsel-imenu)
  ;;        ("C-x l" . counsel-locate))
  ;; :bind (:map read-expression-map
  ;;             ("C-r" . counsel-expression-history)))

(use-package swiper
  :after ivy
  :bind (:map isearch-mode-map
              ("C-o" . swiper-from-isearch)))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/")
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package company
  :commands company-mode
  :bind ("C-<tab>" . company-complete)
  :init
  (setq company-require-match nil)
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 3)
  (setq company-tooltip-flip-when-above t)
  (setq company-show-numbers t)
  :config
  (global-company-mode))

(use-package flycheck
  :defer 5)

(use-package dot-ledger)

(use-package elpy
  :init
  (elpy-enable)
  (defalias 'workon 'pyvenv-workon)
  :config
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-flymake elpy-modules)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (setq elpy-rpc-python-command "python3.6")
  (setq elpy-rpc-backend "jedi")
  (setq python-shell-interpreter "python"
        python-shell-interpreter-args "-i"))

(use-package helm
  :bind (("C-c j" . helm-imenu)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files))
  :config
  (helm-autoresize-mode 1)
  (helm-mode 1))

(use-package helm-config)

(use-package helm-swoop)

(use-package langtool
  :config
  (defvar gn/langtool-path
    (cond ((eq system-type 'windows-nt) (expand-file-name "Applications/langtool/languagetool-commandline.jar" gn-base-dir))
          ((eq system-type 'gnu/linux) (expand-file-name "my/bin/langtool/languagetool-commandline.jar" gn-base-dir))))
  (setq langtool-language-tool-jar gn/langtool-path)
  (setq langtool-default-language "en-US")
  (defun langtool-autoshow-detail-popup (overlays)
    (when (require 'popup nil t)
      ;; Do not interrupt current popup
      (unless (or popup-instances
                  ;; suppress popup after type `C-g` .
                  (memq last-command '(keyboard-quit)))
        (let ((msg (langtool-details-error-message overlays)))
          (popup-tip msg)))))
  (setq langtool-autoshow-message-function 'langtool-autoshow-detail-popup))

(use-package hydra)

(use-package magit
  :commands magit-status
  :bind ("C-c m" . magit-status))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs tab-mark trailing))

  (custom-set-faces
   '(whitespace-tab ((t (:foreground "gray40" :background "#424242"))))))

(use-package deft
  :bind (("<f9>" . deft)
         ("C-c g" . deft-find-file))
  :config
  (setq deft-default-extension "org")
  (setq deft-extensions '("org" "txt" "text" "md" "text" "markdown"))
  (setq deft-directory (expand-file-name "doc/reference/" gn-base-dir))
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase)))
  (setq deft-text-mode 'org-mode)
  (setq deft-auto-save-interval 0.0))

(use-package smooth-scrolling
  :config
  (setq smooth-scroll-margin 5)
  (smooth-scrolling-mode 1))

(use-package calendar
  :custom
  (calendar-week-start-day 1)
  (calendar-location-name "Tomsk")
  (calendar-latitude 56.30)
  (calendar-longitude 84.58)
  :config
  (setq calendar-date-display-form calendar-european-date-display-form))

(use-package ibuffer
  :commands ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (use-package ibuf-ext)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-expert t)
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Dired" (mode . dired-mode))
           ("Planner"
            (or (filename . "\\(todo\\|refile\\|someday\\|archive\*\\).org")
                (mode . org-agenda-mode)
                (name . "^\\*Calendar\\*$")
                (name . "^diary$")))
           ("Text"
            (or (name . "\\.\\(tex\\|bib\\|csv\\)")
                (mode . org-mode)
                (mode . markdown-mode)
                (mode . text-mode)
                (mode . ledger-mode)))
           ("Emacs"
            (or (name . "^\\*scratch\\*$")
                (name . "^\\*Messages\\*$")
                (name . "^\\*Help\\*$")
                (name . "^\\*info\\*$")
                (name . "\*.*\*"))))))
  (defun my-ibuffer-mode-hook ()
    (hl-line-mode 1)
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default"))
  (add-hook 'ibuffer-mode-hook #'my-ibuffer-mode-hook))

(use-package olivetti
  :commands olivetti-mode
  :bind ("<f6>" . olivetti-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (when (eq system-type 'windows-nt)
    (setq markdown-command "multimarkdown.exe"))
  (add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'markdown-mode-hook 'turn-on-olivetti-mode))

(use-package ispell
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . gn/toggle-ispell-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i r" . ispell-region)
         ("C-c i v" . ispell-buffer))
  :commands ispell-word
  :config
  (add-to-list 'ispell-local-dictionary-alist
               '("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8))
  (add-to-list 'ispell-local-dictionary-alist
               '("russian" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "ru_RU") nil koi8-r))
  (setq ispell-dictionary "english")
  (setq ispell-silently-savep t)
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))

  (defun gn/toggle-ispell-dictionary ()
    "Switch russian and english dictionaries."
    (interactive)
    (let* ((dict ispell-current-dictionary)
           (new (if (string= dict "russian") "english"
                  "russian")))
      (ispell-change-dictionary new)
      (message "Switched dictionary from %s to %s" dict new))))

(use-package flyspell
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i m" . flyspell-mode))
  :config
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))
  (bind-key "C-;" #'flyspell-popup-correct flyspell-mode-map)
  (bind-key "C-:" #'flyspell-check-next-highlighted-word flyspell-mode-map))

(use-package flyspell-popup)

(use-package smart-mode-line
  :config
  ;; See https://github.com/Malabarba/smart-mode-line/issues/217
  (setq mode-line-format (delq 'mode-line-position mode-line-format))
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'light)
  (remove-hook 'display-time-hook 'sml/propertize-time-string))

(use-package smartparens-config
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package dired
  :config
  (use-package dired-x)
  (use-package dired+
    :config
    (setq diredp-hide-details-initially-flag nil)
    (diredp-toggle-find-file-reuse-dir 1))

  (add-hook 'dired-mode-hook (lambda () (hl-line-mode 1)))
  (setq dired-omit-files "^\\...+$")
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
  (cond ((eq system-type 'gnu/linux)
         (setq dired-listing-switches
               "-aBhl --group-directories-first"))
        ((eq system-type 'windows-nt)
         (setq dired-listing-switches "-alh"))))

(cond ((eq system-type 'gnu/linux)
       ;; FIXME fix the font changing in GUI on Linux
       ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25228
       (defalias 'dynamic-setting-handle-config-changed-event 'ignore)
       (define-key special-event-map [config-changed-event] #'ignore)
       ;; (add-to-list 'initial-frame-alist '(font . "Meslo LG M 11"))
       ;; (add-to-list 'default-frame-alist '(font . "Meslo LG M 11"))
       (set-face-attribute 'default nil
                           :family "Meslo LG M"
                           :height 115)

       (use-package zenburn-theme
         :config
         (load-theme 'zenburn t)))

      ((eq system-type 'windows-nt)
       (add-to-list 'default-frame-alist '(font . "Meslo LG S 11"))
       (setq default-directory gn-base-dir)
       (use-package w32-browser)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
(load-file "~/.emacs.d/personal.el")

(use-package dot-org)

(require 'server)
(or (server-running-p) (server-start))

;;; init.el ends here
