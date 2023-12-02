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

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
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
(straight-use-package 'cape)
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'marginalia)
(straight-use-package 'ag)
(straight-use-package 'nginx-mode)
(straight-use-package 'csv-mode)
(straight-use-package 'treemacs)
(straight-use-package 'avy)
(straight-use-package 'elfeed)
(straight-use-package 'bind-key)
(straight-use-package 'deft)
(straight-use-package 'diminish)
(straight-use-package 'docker-compose-mode)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'elpy)
(straight-use-package 'find-file-in-project)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-ledger)
(straight-use-package 'flyspell-popup)
(straight-use-package 'js2-mode)
(straight-use-package 'langtool)
(straight-use-package 'ledger-mode)
(straight-use-package 'magit)
(straight-use-package 'markdown-mode)
(straight-use-package 'olivetti)
(straight-use-package 'org)
(straight-use-package 'org-contrib)
(straight-use-package 'ox-clip)
(straight-use-package 'ox-pandoc)
(straight-use-package 'smartparens)
(straight-use-package 'smex)
(straight-use-package 'sokoban)
(straight-use-package 'which-key)
(straight-use-package 'w32-browser)
(straight-use-package 'web-mode)
(straight-use-package 'yasnippet)
(straight-use-package 'zenburn-theme)

(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-pyright)

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

(setq native-comp-async-report-warnings-errors 'silent)

(blink-cursor-mode -1)
(tooltip-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default display-fill-column-indicator-column 79)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
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
(global-auto-revert-mode 1)
(setq enable-recursive-minibuffers t)
(setq auth-source-save-behavior nil)
(setq kill-whole-line t)
(setq-default indicate-empty-lines t)

(setq bookmark-set-fringe-mark nil)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; C style
(setq c-default-style "linux"
      c-basic-offset 4)

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

(defun gn/disable-company-mode-hook ()
  (company-mode -1))

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
            `(whitespace-tab ((t (:foreground "gray40" :background "#424242"))))
            `(ivy-current-match ((t (:background ,zenburn-bg+1 :underline nil))))
            `(swiper-line-face ((t (:background ,zenburn-bg+1 :underline nil))))))
         (setq zenburn-add-font-lock-keywords t)))

      (*is-windows*
       (set-face-attribute 'mode-line nil :box nil)
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

(use-package lsp-mode
  :disabled t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :disabled t
  :commands lsp-ui-mode)

(use-package lsp-pyright
  :disabled t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(use-package recentf
  :config
  (add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude "AppData/Local/Temp")
  (setq recentf-max-saved-items 100)
  (recentf-mode 1))

;; Enable vertico
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
                ("RET" .           vertico-directory-enter)
                )))

;; Optionally use the `orderless' completion style.
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

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
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

  ;; The :init configuration is always executed (Not lazy)
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

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

(autoload #'tramp-register-crypt-file-name-handler "tramp-crypt")
(use-package tramp)

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
        '(("https://www.opennet.ru/opennews/opennews_all.rss" software)
          ("https://lwn.net/headlines/newrss" software)
          ("https://dictionaryblog.cambridge.org/feed/" english)
          ("https://dxdt.ru/feed/" software)
          ("http://lleo.me/dnevnik/rss.xml" blogs)
          ("http://ammo1.livejournal.com/data/rss" blogs)
          ("https://mikrotik.com/current.rss" software)
          ("https://blog.mikrotik.com/rss/" software)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCi8e0iOVk1fEOogdfu4YgfA" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSc16oMxxlcJSb9SXkjwMjA" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCWiY6fYdxuEe78r-0uFCnhA" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbhMGG0ZievPtK8mzLH5jhQ" youtube)
          ("https://www.youtube.com/feeds/videos.xml?user=TheBadComedian" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCes1EvRjcKU4sY_UEavndBw" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCZ-ix1fUTguJvwj6sxgF-6A" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRI00CwLZdLRCWg5BdDOsNw" youtube)))

  (setq elfeed-search-filter "@2-days-ago +unread"
        elfeed-search-title-max-width 75)

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
  (completion-cycle-threshold nil)

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

(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

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
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-mode 1))

(use-package css-mode
  :config
  (setq-default css-indent-offset 2))

(use-package ibuffer
  :commands ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (defun my-ibuffer-mode-hook ()
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default")
    (setq ibuffer-hidden-filter-groups (list "Other" "Emacs"))
    (hl-line-mode 1)
    (ibuffer-update nil t))
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
  ;;:hook (dired-mode . dired-hide-details-mode)
  :config
  (setq font-lock-maximum-decoration (quote ((dired-mode . t) (t . t))))
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

(use-package nginx-mode
  :config
  (add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

(use-package gnus
  :init
  (setq gnus-init-file (expand-file-name "init-gnus" user-emacs-directory)
        gnus-home-directory "~/my/gnus"))

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
