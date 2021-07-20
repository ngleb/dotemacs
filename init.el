;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Init file

;;; Code:

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 16777216
                   gc-cons-percentage 0.1)))

(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (eq system-type 'windows-nt))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)

(setq package-pinned-packages
      '(;; list of packages to be installed
        (ag . "melpa")
        (nginx-mode . "melpa")
        (csv-mode . "gnu")
        (treemacs . "melpa")
        (tramp . "gnu")
        (avy . "melpa")
        (elfeed . "melpa-stable")
        (async . "melpa-stable")
        (bind-key . "melpa")
        (company . "melpa-stable")
        (counsel . "melpa-stable")
        (dash . "melpa")
        (deft . "melpa")
        (diminish . "melpa")
        (docker-compose-mode . "melpa")
        (dockerfile-mode . "melpa")
        (elpy . "melpa-stable")
        (find-file-in-project . "melpa-stable")
        (flycheck . "melpa")
        (flycheck-ledger . "melpa")
        (flyspell-popup . "melpa")
        (git-commit . "melpa-stable")
        (helm . "melpa")
        (helm-core . "melpa")
        (helm-org . "melpa")
        (helm-swoop . "melpa")
        (helm-descbinds . "melpa")
        (highlight-indentation . "melpa-stable")
        (hydra . "melpa")
        (ivy . "melpa-stable")
        (js2-mode . "melpa")
        (langtool . "melpa")
        (ledger-mode . "melpa")
        (magit . "melpa-stable")
        (markdown-mode . "melpa")
        (olivetti . "melpa")
        (org . "gnu")
        (org-contrib . "nongnu")
        (ox-clip . "melpa")
        (ox-pandoc . "melpa")
        (popup . "melpa-stable")
        (pyvenv . "melpa-stable")
        (smartparens . "melpa")
        (smex . "melpa")
        (sokoban . "gnu")
        (swiper . "melpa-stable")
        (use-package . "melpa")
        (which-key . "melpa")
        (w32-browser . "melpa")
        (web-mode . "melpa")
        (with-editor . "melpa-stable")
        (yasnippet . "melpa")
        (zenburn-theme . "melpa")))

(when (version< emacs-version "27.0") (package-initialize))
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

(setq use-package-compute-statistics t)

(require 'bind-key)
(require 'cl-lib)
(require 'diminish)

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(defconst gn-base-dir
  (file-name-as-directory
   (pcase system-type
     (`gnu/linux (expand-file-name "~"))
     (`windows-nt (getenv "USERPROFILE")))))

(blink-cursor-mode -1)
(tooltip-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(when (version<= "26.0.50" emacs-version)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))
(setq mouse-highlight nil)
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
(setq require-final-newline nil)
(setq sentence-end-double-space nil)
(setq-default truncate-lines t)
(setq-default word-wrap t)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(global-auto-revert-mode 1)
(setq enable-recursive-minibuffers t)
(setq auth-source-save-behavior nil)
(setq kill-whole-line t)
(setq-default indicate-empty-lines t)

(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Stop scrolling by huge leaps
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position t
      scroll-margin 0
      hscroll-margin 1
      hscroll-step 1)

;; MULE & encoding setup
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system
 (if *is-windows*
     'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
   'utf-8))
(prefer-coding-system 'utf-8)
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

(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

(defun expose (function &rest args)
  "Return an interactive version of FUNCTION, 'exposing' it to the user."
  (lambda ()
    (interactive)
    (apply function args)))

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

(cond (*is-linux*
       ;; TODO fix the font changing in GUI on Linux
       ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25228
       (defalias 'dynamic-setting-handle-config-changed-event 'ignore)
       (define-key special-event-map [config-changed-event] #'ignore)
       (set-face-attribute 'default nil
                           :family "Meslo LG M"
                           :height 115)
       (use-package zenburn-theme
         :config
         (load-theme 'zenburn t)
         (zenburn-with-color-variables
           (custom-theme-set-faces
            'zenburn
            `(whitespace-tab ((t (:foreground "gray40" :background "#424242"))))
            `(ivy-current-match ((t (:background ,zenburn-bg+1 :underline nil))))
            `(swiper-line-face ((t (:background ,zenburn-bg+1 :underline nil))))))
         (setq zenburn-add-font-lock-keywords t)))

      (*is-windows*
       (set-face-attribute 'mode-line nil :box nil)
       (add-to-list 'default-frame-alist '(font . "Meslo LG S 11"))
       (setq inhibit-compacting-font-caches t)
       (setq default-directory gn-base-dir)
       (use-package w32-browser)))

(use-package helm-config
  :disabled t)

(use-package helm-mode
  :disabled t
  :demand
  :after helm-config
  :bind (("M-x" . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-c j" . helm-imenu)
         ("C-x B" . helm-mini)
         ("C-x c o" . helm-occur))
  :config
  (setq helm-ff-cache-mode-lighter-sleep ""
        helm-ff-cache-mode-lighter-updating "")
  (setq helm-mode-handle-completion-in-region nil)
  (setq helm-display-header-line nil)
  (setq helm-split-window-inside-p t)
  (setq helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
  (add-to-list 'helm-boring-buffer-regexp-list (rx "magit"))
  (add-to-list 'helm-boring-buffer-regexp-list (rx "*Flycheck"))

  (when *is-windows*
    (setq helm-locate-command "es %s -sort run-count %s")
    (defun helm-es-hook ()
      (when (and (equal (assoc-default 'name (helm-get-current-source)) "Locate")
                 (string-match "\\`es" helm-locate-command))
        (mapc (lambda (file)
                (call-process "es" nil nil nil
                              "-inc-run-count" (convert-standard-filename file)))
              (helm-marked-candidates))))
    (add-hook 'helm-find-many-files-after-hook 'helm-es-hook))

  (helm-mode 1)
  (helm-autoresize-mode 1))

(use-package helm-descbinds
  :disabled t
  :after helm-mode
  :config
  (helm-descbinds-mode 1))

(use-package helm-swoop
  :disabled t
  :after helm-mode
  :bind ("C-x c s" . helm-swoop)
  :config
  (setq helm-swoop-speed-or-color t))

(use-package helm-org
  :disabled t
  :after (org helm-mode)
  :bind (:map org-mode-map
              ("C-c j" . helm-org-in-buffer-headings))
  :config
  (setq helm-org-format-outline-path t))

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
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

(use-package youtube-dl
  :config
  (setq youtube-dl-arguments '("--no-mtime" "--format" "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best")
        youtube-dl-directory (expand-file-name "videos/videos" gn-base-dir)))

(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
        '(("https://softwaremaniacs.org/blog/feed/" blogs)
          ("https://kg-portal.ru/rss/news_all.rss" movies)
          ("https://feeds.feedburner.com/smartfiction" books)
          ("https://www.opennet.ru/opennews/opennews_all.rss" software)
          ("https://planet.gentoo.org/rss20.xml" software)
          ("https://lwn.net/headlines/newrss" software)
          ("http://www.buchman.co.il/feed/" marketing)
          ("https://dictionaryblog.cambridge.org/feed/" english)
          ("https://feeds.feedburner.com/arstechnica/index/" news)
          ("https://dxdt.ru/feed/" software)
          ("http://jimblog.me/?feed=atom" blogs)
          ("https://www.smashingmagazine.com/feed/" blogs)
          ("http://www.stayclassicblog.com/feed/atom/" blogs)
          ("http://lleo.me/dnevnik/rss.xml" blogs)
          ("https://postnauka.ru/feed" news)
          ("http://nullprogram.com/feed/" software)
          ("http://ammo1.livejournal.com/data/rss" blogs)
          ("https://mikrotik.com/current.rss" software)
          ("https://anchor.fm/s/29b5580/podcast/rss" marketing)
          ("https://www.allthingsdistributed.com/index.xml" marketing)
          ("https://blog.mikrotik.com/rss/" software)
          ("https://planet.emacslife.com/atom.xml" software)
          ("https://nemihail.livejournal.com/data/atom" blogs)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCi8e0iOVk1fEOogdfu4YgfA" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCB_qr75-ydFVKSF9Dmo6izg" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSc16oMxxlcJSb9SXkjwMjA" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCg0Y6Q0m3A_5X0CPY-IG3Yg" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCWiY6fYdxuEe78r-0uFCnhA" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbhMGG0ZievPtK8mzLH5jhQ" youtube)
          ("https://www.youtube.com/feeds/videos.xml?user=TheBadComedian" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCyNtlmLB73-7gtlBz00XOQQ" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRI00CwLZdLRCWg5BdDOsNw" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCes1EvRjcKU4sY_UEavndBw" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZ-ix1fUTguJvwj6sxgF-6A" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsKiNBoIWLpIxU6vsAv3v3w" youtube)))

  (setq elfeed-search-filter "@2-days-ago +unread "
        elfeed-search-title-max-width 95)

  (defun elfeed-show-youtube-dl ()
    "Download the current entry with youtube-dl."
    (interactive)
    (pop-to-buffer (youtube-dl (elfeed-entry-link elfeed-show-entry))))

  (cl-defun elfeed-search-youtube-dl (&key slow)
    "Download the current entry with youtube-dl."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (dolist (entry entries)
        (if (null (youtube-dl (elfeed-entry-link entry)
                              :title (elfeed-entry-title entry)
                              :slow slow))
            (message "Entry is not a YouTube link!")
          (message "Downloading %s" (elfeed-entry-title entry)))
        (elfeed-untag entry 'unread)
        (elfeed-search-update-entry entry)
        (unless (use-region-p) (forward-line)))))

  (defalias 'elfeed-search-youtube-dl-slow
    (expose #'elfeed-search-youtube-dl :slow t))

  (define-key elfeed-show-mode-map "d" 'elfeed-show-youtube-dl)
  (define-key elfeed-search-mode-map "d" 'elfeed-search-youtube-dl)
  (define-key elfeed-search-mode-map "D" 'elfeed-search-youtube-dl-slow)
  (define-key elfeed-search-mode-map "L" 'youtube-dl-list)

  (define-key elfeed-search-mode-map "h"
    (lambda ()
      (interactive)
      (elfeed-search-set-filter (default-value 'elfeed-search-filter))))

  (define-key elfeed-search-mode-map (kbd "l")
    (lambda ()
      (interactive)
      (switch-to-buffer (elfeed-log-buffer))))

  (define-key elfeed-search-mode-map "t"
    (lambda ()
      (interactive)
      (cl-macrolet ((re (re rep str) `(replace-regexp-in-string ,re ,rep ,str)))
        (elfeed-search-set-filter
         (cond
          ((string-match-p "-youtube" elfeed-search-filter)
           (re " *-youtube" " +youtube" elfeed-search-filter))
          ((string-match-p "\\+youtube" elfeed-search-filter)
           (re " *\\+youtube" " -youtube" elfeed-search-filter))
          ((concat elfeed-search-filter " -youtube")))))))

  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
                                :remove 'unread))

  (defface elfeed-youtube
    '((t :foreground "#f9f"))
    "Marks YouTube videos in Elfeed."
    :group 'elfeed)

  (push '(youtube elfeed-youtube)
        elfeed-search-face-alist))

(use-package man
  :config
  (bind-key "j" (kbd "C-u 1 C-v") Man-mode-map)
  (bind-key "k" (kbd "C-u 1 M-v") Man-mode-map))

(use-package ivy
  :demand t
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-do-completion-in-region nil)
  (setq ivy-height 10)
  (ivy-mode 1))

(use-package counsel
  :demand t
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-c i i" . counsel-imenu)
         ("C-x C-f" . counsel-find-file)
         ("M-y" . counsel-yank-pop)
         ("C-x r b" . counsel-bookmark)
         ("C-c o" . counsel-outline)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h b" . counsel-descbinds)
         ("C-*" . counsel-org-agenda-headlines)
         ("C-x l" . counsel-locate))
  :config
  (counsel-mode 1))
  
(use-package swiper
  :after ivy
  :demand t
  :commands iswiper-isearch
  :bind ("C-s" . swiper-isearch))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/")
  (setq uniquify-ignore-buffers-re "^\\*"))

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
  (global-flycheck-mode)
  :config
  (defalias 'show-error-at-point-soon
    'flycheck-show-error-at-point))

(use-package company
  :diminish " ❋"
  :bind(:map company-mode-map
        ("TAB" . company-indent-or-complete-common)
        :map company-active-map
        ("TAB" . company-complete-common-or-cycle)
        ("<tab>" . company-complete-common-or-cycle))
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-require-match nil
        company-show-numbers t
        company-minimum-prefix-length 2
        company-idle-delay 0.1))

(use-package find-file-in-project)

(use-package pyvenv
  :init
  (defalias 'workon 'pyvenv-workon))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-interpreter "python"
        python-shell-interpreter-args "-i")
  (setq python-indent-guess-indent-offset nil))

(use-package elpy
  :after python
  :config
  (setq elpy-remove-modeline-lighter nil)
  (setq elpy-rpc-virtualenv-path 'current)
  (setq elpy-shell-echo-input nil)
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-flymake elpy-modules)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (elpy-enable))

(use-package which-key
  :config
  (setq which-key-idle-delay 1.5)
  (which-key-mode))

(use-package hydra)

(use-package magit
  :commands magit-status
  :bind ("C-c m" . magit-status))

(use-package ielm
  :defer t
  :config
  (define-key ielm-map (kbd "C-c C-z") #'quit-window))

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs tab-mark trailing)))

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
  :config
  (setq calendar-week-start-day 1
        calendar-location-name "Tomsk"
        calendar-latitude 56.30
        calendar-longitude 84.58
        calendar-mark-holidays-flag t)
  (setq calendar-date-display-form calendar-european-date-display-form))

(use-package web-mode
  :mode ("\\.html?\\'" . web-mode))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (add-to-list 'flycheck-disabled-checkers #'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-mode 1))

(use-package ibuffer
  :commands ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (defun my-ibuffer-mode-hook ()
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default")
    (hl-line-mode 1))
  (add-hook 'ibuffer-mode-hook #'my-ibuffer-mode-hook)
  :config
  (use-package ibuf-ext)
  (add-to-list 'ibuffer-never-show-predicates "^\\*helm")
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-expert t)
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Dired" (mode . dired-mode))
           ("Planner"
            (or (filename . "\\(gtd\\|todo\\|refile\\|reading\\|mobile\\|someday\\|purchases\\|archive\*\\).org")
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
                (name . "^\\*info\\*$")))
           ("Other"
            (or (name . "\*.*\*")
                (name . "^magit.*"))))))
  ;; nearly all of this is the default layout
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 30 30 :left :elide) ; change: 30s were originally 18s
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename))))

(use-package olivetti
  :commands olivetti-mode
  :bind ("C-c i o" . olivetti-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package ispell
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . gn/toggle-ispell-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i r" . ispell-region)
         ("C-c i v" . ispell-buffer))
  :commands ispell-word
  :init
  (setq ispell-dictionary "english")
  :config
  (add-to-list 'ispell-local-dictionary-alist
               '("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8))
  (add-to-list 'ispell-local-dictionary-alist
               '("russian" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "ru_RU") nil koi8-r))
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
         ("C-c i f" . flyspell-mode))
  :config
  (use-package flyspell-popup)
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))
  (bind-key "C-;" #'flyspell-popup-correct flyspell-mode-map)
  (bind-key "C-:" #'flyspell-check-next-highlighted-word flyspell-mode-map))

(use-package langtool
  :config
  (cond (*is-linux*
         (setq langtool-bin "/usr/bin/languagetool"))
        (*is-windows*
         (setq langtool-language-tool-jar "C:/apps/langtool/languagetool-commandline.jar")))
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

(use-package ls-lisp
  :config
  (when *is-windows*
    (setq ls-lisp-format-time-list  '("%d.%m.%Y %H:%M" "%d.%m.%Y %H:%M"))
    (setq ls-lisp-emulation 'MS-Windows)
    (ls-lisp-set-options)))

(use-package dired
  :bind (:map dired-mode-map
              ("l" . dired-up-directory))
  :hook (dired-mode . hl-line-mode)
;;  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq font-lock-maximum-decoration (quote ((dired-mode . nil) (t . t))))
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-dwim-target t)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-hide-details-hide-information-lines nil)
  (defconst my-dired-listing-switches
    (pcase system-type
      (`gnu/linux "-ahl --group-directories-first")
      (`windows-nt "-alh")))
  (setq dired-listing-switches my-dired-listing-switches))

(use-package dired-x
  :after dired
  :bind (:map dired-mode-map
              ("h" . dired-omit-mode))
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package dired+
  :after dired-x
  :init
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-hide-details-propagate-flag t)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package recentf
  :config
  (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude "AppData/Local/Temp")
  (setq recentf-max-saved-items 100))

(use-package nginx-mode
  :config
  (add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

(use-package gnus
  :init
  (setq gnus-init-file (expand-file-name "init-gnus" user-emacs-directory)
        gnus-home-directory "~/my/gnus"))

(use-package savehist
  :config
  (setq history-delete-duplicates t)
  (savehist-mode 1))

(use-package server
  :config (or (server-running-p) (server-mode)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(load "~/.emacs.d/personal")
(load "~/.emacs.d/init-org")
(load "~/.emacs.d/init-ledger")


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
    (`office 220)))

(defconst emacs-min-height
  (pcase display-name
    (`lenovo 40)
    (`lenovo-m 53)
    (`office 50)))

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

(defun emacs-maximize ()
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized))

(defun emacs-toggle-size ()
  (interactive)
  (if (alist-get 'fullscreen (frame-parameters))
      (emacs-min)
    (emacs-max)))

(add-hook 'emacs-startup-hook #'emacs-maximize t)
(bind-key "C-<f12>" #'emacs-toggle-size)



(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
