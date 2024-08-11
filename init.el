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

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(straight-use-package '(vertico :files (:defaults "extensions/*")
                                :includes (vertico-buffer
                                           vertico-directory
                                           vertico-flat
                                           vertico-indexed
                                           vertico-mouse
                                           vertico-quick
                                           vertico-repeat
                                           vertico-reverse)))
(straight-use-package '(corfu :files (:defaults "extensions/*")
                              :includes (corfu-echo       ;; corfu-echo-mode displays a brief candidate documentation in the echo area.
                                         corfu-history    ;; corfu-history-mode remembers selected candidates and sorts the candidates by their history position.
                                         corfu-indexed    ;; corfu-indexed-mode allows you to select indexed candidates with prefix arguments.
                                         corfu-info       ;; Actions to access the candidate location and documentation.
                                         corfu-popupinfo  ;; Display candidate documentation or source in a popup next to the candidate menu.
                                         corfu-quick)))      ;; Commands to select using Avy-style quick keys.)
(straight-use-package 'ag)
(straight-use-package 'avy)
(straight-use-package 'bind-key)
(straight-use-package 'cape)
(straight-use-package 'company)
(straight-use-package 'consult)
(straight-use-package 'csv-mode)
(straight-use-package 'dap-mode)
(straight-use-package 'deft)
(straight-use-package 'diminish)
(straight-use-package 'docker-compose-mode)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'elfeed)
(straight-use-package 'elpy)
(straight-use-package 'find-file-in-project)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-ledger)
(straight-use-package 'flyspell-popup)
(straight-use-package 'js2-mode)
(straight-use-package 'langtool)
(straight-use-package 'ledger-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'magit)
(straight-use-package 'marginalia)
(straight-use-package 'markdown-mode)
(straight-use-package 'nix-mode)
(straight-use-package 'nginx-mode)
(straight-use-package 'olivetti)
(straight-use-package 'orderless)
(straight-use-package 'org)
(straight-use-package 'org-contrib)
(straight-use-package 'ox-clip)
(straight-use-package 'ox-pandoc)
(straight-use-package 'smartparens)
(straight-use-package 'smex)
(straight-use-package 'sokoban)
(straight-use-package 'treemacs)
(straight-use-package 'w32-browser)
(straight-use-package 'web-mode)
(straight-use-package 'which-key)
(straight-use-package 'yasnippet)
(straight-use-package 'zenburn-theme)

(eval-when-compile
  (require 'use-package))

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
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default display-fill-column-indicator-column 79)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(setopt mouse-highlight nil)
(setopt use-short-answers t)
(setopt initial-scratch-message nil)
(setopt inhibit-splash-screen t)
(setopt inhibit-startup-message t)
(setopt confirm-kill-emacs 'y-or-n-p)
(setopt sentence-end-double-space nil)
(setopt enable-recursive-minibuffers t)
(setopt kill-whole-line t)
(setopt indicate-empty-lines t)
(setopt scroll-conservatively 101)
(setopt native-comp-async-report-warnings-errors nil)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

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
(setopt default-input-method "russian-computer")

;; Stop creating backup and auto-save files
(setopt make-backup-files nil) ; stop creating those backup~ files
(setopt auto-save-default nil) ; stop creating those #auto-save# files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

(bind-key "C-x k" #'kill-this-buffer)
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
       ;; ;; TODO fix the font changing in GUI on Linux
       ;; ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25228
       ;; (add-to-list 'default-frame-alist '(font . "Meslo LG M 11"))
       (defalias 'dynamic-setting-handle-config-changed-event 'ignore)
       (define-key special-event-map [config-changed-event] #'ignore)
       (set-face-attribute 'default nil
                           :family "JetBrains Mono 11"
                           :height 115)
       (use-package zenburn-theme
         :config
         (load-theme 'zenburn t)
         (zenburn-with-color-variables
           (custom-theme-set-faces
            'zenburn
            `(whitespace-tab ((t (:foreground "gray40" :background "#424242"))))))
         (setq zenburn-add-font-lock-keywords t)))

      (*is-windows*
       (add-to-list 'default-frame-alist '(font . "Meslo LG S 12"))
       (setq inhibit-compacting-font-caches t)
       (setq default-directory gn-base-dir)
       (use-package w32-browser)))

(use-package avy
  :bind* ("C-." . avy-goto-char-timer)
  :config
  (avy-setup-default))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package recentf
  :custom
  (recentf-max-saved-items 200)
  :config
  (recentf-mode))

(use-package vertico
  :demand t
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode)
  (use-package vertico-directory
    :demand t
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
    :bind (:map vertico-map
                ("<backspace>"   . vertico-directory-delete-char)
                ("C-w"           . vertico-directory-delete-word)
                ("C-<backspace>" . vertico-directory-delete-word)
                ("RET" .           vertico-directory-enter))))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-c i i" . consult-info)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x b" . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("M-y" . consult-yank-pop)            ;; orig. yank-pop
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; M-g bindings in `goto-map'
         ("M-g g" . consult-goto-line)           ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)             ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s f" . consult-fd)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map org-mode-map
         ("C-c o" . consult-org-heading)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  (consult-narrow-key "<")

  :functions
  (consult-register-format
   consult-register-window
   consult-xref)

  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (use-package consult-xref)
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-recent-file
   consult-xref
   consult--source-bookmark
   consult--source-file-register
   consult--source-recent-file
   consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package youtube-dl
  :config
  (setq youtube-dl-arguments '("--no-mtime" "--format" "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best")
        youtube-dl-directory (expand-file-name "videos/videos" gn-base-dir)))

(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
        '(("https://www.opennet.ru/opennews/opennews_all.rss" software)
          ("https://lwn.net/headlines/newrss" software)
          ("https://dictionaryblog.cambridge.org/feed/" english)
          ("https://dxdt.ru/feed/" software)
          ("http://lleo.me/dnevnik/rss.xml" blogs)
          ("http://ammo1.livejournal.com/data/rss" blogs)
          ("https://mikrotik.com/current.rss" software)
          ("https://download.mikrotik.com/routeros/latest-stable-and-long-term.rss" software)
          ("https://kg-portal.ru/rss/news_all.rss" news)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCi8e0iOVk1fEOogdfu4YgfA" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSc16oMxxlcJSb9SXkjwMjA" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCWiY6fYdxuEe78r-0uFCnhA" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbhMGG0ZievPtK8mzLH5jhQ" youtube)
          ("https://www.youtube.com/feeds/videos.xml?user=TheBadComedian" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCes1EvRjcKU4sY_UEavndBw" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZ-ix1fUTguJvwj6sxgF-6A" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRI00CwLZdLRCWg5BdDOsNw" youtube)))

  (setq elfeed-search-filter "@3-days-ago +unread"
        elfeed-search-title-max-width 75
        elfeed-curl-extra-arguments '("-xhttp://127.0.0.1:8080"))

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
  (add-hook 'elfeed-show-mode-hook
            (lambda ()
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (setq-local truncate-lines nil)
                (setq-local shr-width 70)
                (set-buffer-modified-p nil))
              (set-face-attribute 'variable-pitch (selected-frame) :font (font-spec :family "Noto Sans" :size 16))
              (setq-local left-margin-width 55)
              (setq-local right-margin-width 55)
              ))

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

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/")
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package corfu
  :demand t
  :bind (("M-/" . completion-at-point)
         :map corfu-map
         ("C-n"      . corfu-next)
         ("C-p"      . corfu-previous)
         ("<escape>" . corfu-quit)
         ("<return>" . corfu-insert)
         ("M-d"      . corfu-info-documentation)
         ("M-l"      . corfu-info-location)
         ("M-."      . corfu-move-to-minibuffer))
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold 3)

  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)

  (corfu-echo-documentation nil)

  (corfu-quit-at-boundary nil)
  (corfu-separator ?\s)            ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  (corfu-preview-current 'insert)  ; Preview first candidate. Insert on input if only one
  (corfu-preselect-first t)        ; Preselect first candidate?

  :config
  (global-corfu-mode))

(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :bind (:map corfu-map
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-p" . corfu-popupinfo-scroll-down)
              ([remap corfu-show-documentation] . corfu-popupinfo-toggle))
  :custom
  (corfu-popupinfo-delay 0.5)
  (corfu-popupinfo-max-width 70)
  (corfu-popupinfo-max-height 20)
  (corfu-echo-documentation nil))

(use-package find-file-in-project)

(use-package pyvenv
  :init
  (defalias 'workon 'pyvenv-workon)
  :config
  (setenv "WORKON_HOME" (expand-file-name "~/dev/envs/"))
  (pyvenv-mode t))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-interpreter "python"
        python-shell-interpreter-args "-i")
  (setq python-indent-guess-indent-offset nil))

;; (use-package elpy
;;   :after python
;;   :config
;;   (setq elpy-remove-modeline-lighter nil)
;;   (setq elpy-rpc-virtualenv-path 'current)
;;   (setq elpy-shell-echo-input nil)
;;   (delete 'elpy-module-highlight-indentation elpy-modules)
;;   (delete 'elpy-module-flymake elpy-modules)
;;   (add-hook 'elpy-mode-hook 'flycheck-mode)
;;   (elpy-enable))

(use-package which-key
  :config
  (setq which-key-idle-delay 1.5)
  (which-key-mode))

(use-package hydra)

(use-package magit
  :commands magit-status
  :bind ("C-c m" . magit-status))

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs tab-mark trailing))
  (global-whitespace-mode))

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
  :mode ("\\.html?\\'" . web-mode)
  :custom
    (web-mode-markup-indent-offset 2)
    (web-mode-css-indent-offset 2)
    (web-mode-code-indent-offset 2)
    (web-mode-enable-auto-closing nil))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (add-to-list 'flycheck-disabled-checkers #'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

(use-package css-mode
  :config
  (setq-default css-indent-offset 2))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((js2-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (python-mode . lsp))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package dap-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (use-package dap-firefox))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-formats
   '((mark modified read-only " "
           (name 30 30 :left :elide) ; change: 30s were originally 18s
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename)))
  (ibuffer-saved-filter-groups
   '(("default"
      ("Dired" (mode . dired-mode))
      ("Planner"
       (or
        (filename . "\\(gtd\\|todo\\|refile\\|reading\\|mobile\\|someday\\|purchases\\|archive\*\\).org")
        (mode . org-agenda-mode)
        (name . "^\\*Calendar\\*$")
        (name . "^diary$")))
      ("Text"
       (or
        (name . "\\.\\(tex\\|bib\\|csv\\)")
        (mode . org-mode)
        (mode . markdown-mode)
        (mode . text-mode)
        (mode . ledger-mode)))
      ("Emacs"
       (or
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")
        (name . "^\\*Help\\*$")
        (name . "^\\*info\\*$")))
      ("Other"
       (or
        (name . "\*.*\*")
        (name . "^magit.*"))))))
  :config
  (add-hook 'ibuffer-mode-hook #'hl-line-mode)
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))

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
         ("C-c i f" . flyspell-mode)
         :map flyspell-mode-map
         ("C-;" . flyspell-popup-correct))
  :config
  (use-package flyspell-popup))

(use-package langtool
  :config
  (cond (*is-linux*
         (setq langtool-bin "languagetool-commandline"))
        (*is-windows*
         (setq langtool-language-tool-jar "C:/apps/langtool/languagetool-commandline.jar")))
  (setq langtool-default-language "ru-RU")
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
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . dired-hide-details-mode))
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-hide-details-hide-information-lines nil)
  :config
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

(use-package nginx-mode
  :config
  (add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

(use-package savehist
  :custom
  (savehist-mode t)
  :config
  (savehist-mode 1))

(use-package server
  :config (or (server-running-p) (server-mode)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(load "~/.emacs.d/personal")
(load "~/.emacs.d/init-org")
(load "~/.emacs.d/init-ledger")

(defun emacs-maximize ()
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized))

(add-hook 'emacs-startup-hook #'emacs-maximize t)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
