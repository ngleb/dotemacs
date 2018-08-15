;;; init.el

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
      '(;; list of packages to be installed
        (elfeed . "melpa-stable")
        (async . "melpa-stable")
        (bind-key . "melpa")
        (company . "melpa-stable")
        (counsel . "melpa-stable")
        (dash . "melpa-stable")
        (deft . "melpa")
        (diminish . "melpa")
        (docker-compose-mode . "melpa")
        (dockerfile-mode . "melpa")
        (elpy . "melpa-stable")
        (find-file-in-project . "melpa-stable")
        (flycheck . "melpa-stable")
        (flycheck-ledger . "melpa")
        (flyspell-popup . "melpa-stable")
        (git-commit . "melpa-stable")
        (helm . "melpa")
        (helm-core . "melpa")
        (helm-descbinds . "melpa")
        (helm-describe-modes . "melpa")
        (helm-swoop . "melpa")
        (highlight-indentation . "melpa-stable")
        (hydra . "melpa")
        (ivy . "melpa-stable")
        (js2-mode . "melpa")
        (langtool . "melpa")
        (ledger-mode . "melpa")
        (magit . "melpa-stable")
        (magit-popup . "melpa-stable")
        (markdown-mode . "melpa")
        (nlinum . "gnu")
        (olivetti . "melpa")
        (org-plus-contrib . "org")
        (ox-clip . "melpa")
        (ox-pandoc . "melpa")
        (popup . "melpa-stable")
        (pyvenv . "melpa-stable")
        (smartparens . "melpa")
        (smart-mode-line . "melpa")
        (smex . "melpa")
        (swiper . "melpa-stable")
        (use-package . "melpa")
        (w32-browser . "melpa")
        (web-mode . "melpa")
        (which-key . "melpa")
        (with-editor . "melpa-stable")
        (yasnippet . "melpa")
        (zenburn-theme . "melpa")
        )) ;; end of list

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


(use-package diminish :demand t)


(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(defconst gn-base-dir
  (file-name-as-directory
   (pcase system-type
     (`gnu/linux (expand-file-name "~"))
     (`windows-nt (expand-file-name user-login-name "C:/Users")))))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(tooltip-mode -1)
(setq tooltip-use-echo-area t)
(column-number-mode 1)
(show-paren-mode 1)
(add-hook 'prog-mode-hook 'nlinum-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message nil)
(setq x-underline-at-descent-line t)
(when (eq 'window-system 'x)
  (setq x-wait-for-event-timeout nil))
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq require-final-newline t)
(setq sentence-end-double-space nil)
(setq-default truncate-lines t)
(setq-default word-wrap t)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(global-auto-revert-mode 1)

;; Stop scrolling by huge leaps
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position t)

;; MULE & encoding setup
(setq default-input-method "russian-computer")

;; Stop creating backup and auto-save files
(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #auto-save# files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(global-set-key (kbd "C-x k") #'kill-this-buffer)
(bind-key "M-z" #'zap-up-to-char)
(bind-key "M-N" (kbd "C-u 1 C-v"))
(bind-key "M-P" (kbd "C-u 1 M-v"))
(bind-key "C-c i t" #'toggle-truncate-lines)

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

(use-package ediff
  :config
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
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))

(use-package eldoc
  :diminish
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package elfeed
  :init
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "https://www.linux.org.ru/section-rss.jsp?section=1"
          "https://www.opennet.ru/opennews/opennews_all.rss"
          "https://wordpress.org/news/feed/"
          "http://googlechromereleases.blogspot.com/atom.xml"
          "https://planet.gentoo.org/rss20.xml"
          "http://lineageos.org/feed.xml"
          "https://lwn.net/headlines/newrss"
          "http://mynameisangie.com/feed/"
          "http://www.buchman.co.il/feed/"
          "http://www.startup-marketing.com/feed/"
          "https://medium.com/feed/@mwfogleman"
          "https://habr.com/rss/all/all/"
          "https://dictionaryblog.cambridge.org/feed/"
          "https://feeds.feedburner.com/arstechnica/index/"
          "https://dxdt.ru/feed/"
          "http://gettingthingsdone.com/feed/atom/"
          "http://jimblog.me/?feed=atom"
          "https://www.smashingmagazine.com/feed/"
          "http://www.stayclassicblog.com/feed/atom/"
          "http://dolboeb.livejournal.com/data/atom"
          "http://lleo.me/dnevnik/rss.xml"
          "https://postnauka.ru/feed"
          "http://nullprogram.com/feed/"
          "http://planet.emacsen.org/atom.xml"))
  (setq-default elfeed-search-filter "@2-days-ago +unread ")
  :config
  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
                                :remove 'unread))
  (global-set-key (kbd "C-x w") 'elfeed))

(use-package eshell
  :commands (eshell eshell-command))

(use-package isearch)

(use-package man
  :config
  (bind-key "j" (kbd "C-u 1 C-v") Man-mode-map)
  (bind-key "k" (kbd "C-u 1 M-v") Man-mode-map))

(use-package ivy
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  (ivy-do-completion-in-region nil))

(use-package counsel
  :after ivy)

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
  :defer 5
  :commands (company-mode
             company-complete-common-or-cycle)
  :bind (:map company-mode-map
          ("C-M-i" . company-complete-common-or-cycle)
          ("C-<tab>" . company-complete-common-or-cycle)

          :map company-active-map
          ("C-j" . company-complete-selection)
          ("TAB" . company-complete-common-or-cycle)
          ("<tab>" . company-complete-common-or-cycle))
  :init
  (setq company-require-match nil)
  :config
  (diminish 'company-mode " ❋")
  (global-company-mode 1))

(use-package flycheck
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  :init
  (dolist (where '((emacs-lisp-mode-hook . emacs-lisp-mode-map)
                   (js2-mode-hook        . js2-mode-map)
                   (c-mode-common-hook   . c-mode-base-map)
                   (elpy-mode-hook       . elpy-mode-map)))
    (add-hook (car where)
              `(lambda ()
                 (bind-key "M-n" #'flycheck-next-error ,(cdr where))
                 (bind-key "M-p" #'flycheck-previous-error ,(cdr where)))))
  :init
  (global-flycheck-mode)
  :config
  (defalias 'show-error-at-point-soon
    'flycheck-show-error-at-point))

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-interpreter "python"
        python-shell-interpreter-args "-i")
  (setq python-indent-guess-indent-offset nil))

(use-package pyvenv
  :after python
  :init
  (defalias 'workon 'pyvenv-workon))

(use-package elpy
  :after python
  :config
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-flymake elpy-modules)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (elpy-enable))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-c j" . helm-imenu)
         ("C-x b" . helm-mini) ;; helm-mini or helm-buffers-list
         ("C-x C-f" . helm-find-files)
         ("C-x r b" . helm-bookmarks))
  :config
  (setq helm-split-window-inside-p t)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  (add-to-list 'helm-boring-buffer-regexp-list (rx "*magit-"))
  (add-to-list 'helm-boring-buffer-regexp-list (rx "*Flycheck")))

(use-package helm-config)

(use-package helm-swoop)

(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds)
  :init
  (fset 'describe-bindings 'helm-descbinds))

(use-package helm-describe-modes
  :after helm
  :bind ("C-h C-m" . helm-describe-modes))

(use-package langtool
  :config
  (defconst gn-langtool-path
    (pcase system-type
     (`gnu/linux "Applications/langtool/languagetool-commandline.jar")
     (`windows-nt "my/bin/langtool/languagetool-commandline.jar")))
  (setq langtool-language-tool-jar (expand-file-name gn-langtool-path gn-base-dir))
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

(use-package time
  :config
  (progn
    (setf display-time-24hr-format t
          display-time-day-and-date t)
    (setq display-time-string-forms
          '(month "/" day " " 24-hours ":" minutes " "))
    (display-time-mode t)))

(use-package ielm
  :defer t
  :config
  (define-key ielm-map (kbd "C-c C-z") #'quit-window))

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
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

(use-package calendar
  :custom
  (calendar-week-start-day 1)
  (calendar-location-name "Tomsk")
  (calendar-latitude 56.30)
  (calendar-longitude 84.58)
  (calendar-mark-holidays-flag t)
  :config
  (setq calendar-date-display-form calendar-european-date-display-form))

(use-package web-mode
  :commands web-mode)

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (add-to-list 'flycheck-disabled-checkers #'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-mode 1))

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
            (or (filename . "\\(todo\\|refile\\|reading\\|someday\\|archive\*\\).org")
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
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default")
    (hl-line-mode 1))
  (add-hook 'ibuffer-mode-hook #'my-ibuffer-mode-hook))

(use-package olivetti
  :commands olivetti-mode
  :bind ("C-c i o" . olivetti-mode))

(use-package which-key
  :defer 5
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setf markdown-indent-on-enter nil
        markdown-command
        "pandoc -f markdown -t html5 -s --self-contained --smart")
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

(use-package smartparens-config
  :diminish smartparens-mode
  :config
  ;;(smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package ls-lisp
  :config
  (when (eq system-type 'windows-nt)
    (setq ls-lisp-format-time-list  '("%d.%m.%Y %H:%M" "%d.%m.%Y %H:%M"))
    (setq ls-lisp-emulation 'MS-Windows)
    (ls-lisp-set-options)))

(use-package dired
  :config
  (setq dired-omit-files "^\\...+$")
  (defun my-dired-mode-hook ()
    (hl-line-mode 1)
    (dired-omit-mode 1))
  (add-hook 'dired-mode-hook 'my-dired-mode-hook)

  (defconst my-dired-listing-switches
    (pcase system-type
      (`gnu/linux "-aBhl --group-directories-first")
      (`windows-nt "-alh")))
  (setq dired-listing-switches my-dired-listing-switches))

(use-package dired-x
  :after dired)

(use-package dired+
  :after dired
  :config
  (setq diredp-hide-details-initially-flag nil)
  (diredp-toggle-find-file-reuse-dir 1))

(use-package smex
  :defer 5
  :commands smex)

(use-package recentf
    :config
    (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))
    (setq recentf-max-saved-items 60))

(use-package smart-mode-line
  :config
  ;; See https://github.com/Malabarba/smart-mode-line/issues/217
  (setq mode-line-format (delq 'mode-line-position mode-line-format))
  (setq sml/no-confirm-load-theme t)
  (sml/setup))


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(load "~/.emacs.d/personal")
(load "~/.emacs.d/init-org")
(load "~/.emacs.d/init-ledger")

(require 'server)
(or (server-running-p) (server-start))


(cond ((eq system-type 'gnu/linux)
       ;; TODO fix the font changing in GUI on Linux
       ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25228
       (defalias 'dynamic-setting-handle-config-changed-event 'ignore)
       (define-key special-event-map [config-changed-event] #'ignore)
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


(defconst display-name
  (pcase (display-pixel-height)
    (`768 'lenovo)
    (`1200 'lenovo-m)
    (`1080 'office)))

(defconst emacs-min-top 20)

(defconst emacs-min-left
  (pcase display-name
    (`lenovo 100)
    (`lenovo-m 190)
    (`office 100)))

(defconst emacs-min-height
  (pcase display-name
    (`lenovo 40)
    (`lenovo-m 53)
    (`office 40)))

(defconst emacs-min-width
  (pcase display-name
    (`lenovo 140)
    (`lenovo-m 172)
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

;;; init.el ends here
