;;; init.el
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system
 (if (eq system-type 'windows-nt)
     'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
   'utf-8))
(prefer-coding-system 'utf-8)

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
        (nginx-mode . "melpa")
        (csv-mode . "gnu")
        (treemacs . "melpa")
        (avy . "melpa")
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
        (magit-popup . "melpa-stable")
        (markdown-mode . "melpa")
        (olivetti . "melpa")
        (org-plus-contrib . "org")
        (ox-clip . "melpa")
        (ox-pandoc . "melpa")
        (popup . "melpa-stable")
        (pyvenv . "melpa-stable")
        (smartparens . "melpa")
        (smex . "melpa")
        (sokoban . "gnu")
        (speed-type . "melpa")
        (swiper . "melpa-stable")
        (use-package . "melpa")
        (which-key . "melpa")
        (w32-browser . "melpa")
        (web-mode . "melpa")
        (with-editor . "melpa-stable")
        (yasnippet . "melpa")
        (zenburn-theme . "melpa")
        )) ;; end of list

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

(require 'bind-key)
(require 'cl-lib)
(require 'diminish)


(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(defconst gn-base-dir
  (file-name-as-directory
   (pcase system-type
     (`gnu/linux (expand-file-name "~"))
     (`windows-nt (getenv "USERPROFILE")))))

;; (when (eq system-type 'windows-nt)
;;   (push "C:/msys64/usr/bin" exec-path)
;;   (push "C:/msys64/mingw64/bin" exec-path)
;;   (setenv "PATH" (mapconcat #'identity exec-path path-separator)))

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)(menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
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
(setq require-final-newline t)
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

(use-package avy
  :bind* ("C-." . avy-goto-char-timer)
  :config
  (avy-setup-default))

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

(use-package youtube-dl
  :config
  (setq youtube-dl-arguments '("--no-mtime" "--restrict-filenames" "--format" "bestvideo[ext=mp4][width<=1920][height<=1080]+bestaudio[ext=m4a]/best[ext=mp4]/best")
        youtube-dl-directory (expand-file-name "Downloads" gn-base-dir)))

(use-package elfeed
  :init
  (setq elfeed-feeds
        '(("https://softwaremaniacs.org/blog/feed/" blogs)
          ("https://kg-portal.ru/rss/news.rss" movies)
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
          ("http://tema.livejournal.com/data/rss" blogs)
          ("https://mikrotik.com/current.rss" software)
          ("https://anchor.fm/s/29b5580/podcast/rss" marketing)
          ("https://www.allthingsdistributed.com/index.xml" marketing)
          ("https://blog.mikrotik.com/rss/" software)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCi8e0iOVk1fEOogdfu4YgfA" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCB_qr75-ydFVKSF9Dmo6izg" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCSc16oMxxlcJSb9SXkjwMjA" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCg0Y6Q0m3A_5X0CPY-IG3Yg" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCWiY6fYdxuEe78r-0uFCnhA" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbhMGG0ZievPtK8mzLH5jhQ" youtube)
          ("https://www.youtube.com/feeds/videos.xml?user=TheBadComedian" youtube)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsKiNBoIWLpIxU6vsAv3v3w" youtube)))
  (setq-default elfeed-search-filter "@2-days-ago +unread ")
  (setq elfeed-search-title-max-width 95)

  :config
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

  (global-set-key (kbd "C-x w") 'elfeed)

  (defface elfeed-youtube
    '((t :foreground "#f9f"))
    "Marks YouTube videos in Elfeed."
    :group 'elfeed)

  (push '(youtube elfeed-youtube)
        elfeed-search-face-alist))

(use-package eshell
  :commands (eshell eshell-command))

(use-package man
  :config
  (bind-key "j" (kbd "C-u 1 C-v") Man-mode-map)
  (bind-key "k" (kbd "C-u 1 M-v") Man-mode-map))

(use-package ivy
  :diminish
  :demand t
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-display-style 'fancy)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)
  (ivy-wrap t)
  (ivy-format-function 'ivy-format-function-line))

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
  :diminish " ❋"
  :defer 1
  :bind (:map company-mode-map
         ("C-<tab>" . company-complete-common-or-cycle)
         :map company-active-map
         ("TAB" . company-complete-common-or-cycle)
         ("<tab>" . company-complete-common-or-cycle))
  :config
  (setq company-require-match nil)
  (global-company-mode))

(use-package flycheck
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  :diminish flycheck-mode
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

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-interpreter "python"
        python-shell-interpreter-args "-i")
  (setq python-indent-guess-indent-offset nil))

(use-package which-key
  :defer 3
  :diminish
  :commands which-key-mode
  :init
  (setq which-key-idle-delay 1.5)
  :config
  (which-key-mode))

(use-package flyspell-popup)

(use-package pyvenv
  :init
  (defalias 'workon 'pyvenv-workon))

(use-package elpy
  :after python
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  (setq elpy-shell-echo-input nil)
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-flymake elpy-modules)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (elpy-enable))

(use-package helm
  :defer 1
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-c j" . helm-imenu)
         ("C-x b" . helm-mini) ;; helm-mini or helm-buffers-list
         ("C-x C-f" . helm-find-files)
         ("C-x r b" . helm-bookmarks)
         ("C-x c o" . helm-occur))
  :bind (:map org-mode-map
              ("C-c j" . helm-org-in-buffer-headings))
  :config
  ;; (setq helm-display-function 'helm-display-buffer-in-own-frame
  ;;       helm-display-buffer-reuse-frame t
  ;;       helm-use-undecorated-frame-option t)
  (setq helm-split-window-inside-p t)
  (setq helm-mode-handle-completion-in-region nil)
  (setq helm-org-format-outline-path t)
  (setq helm-display-header-line nil)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  (add-to-list 'helm-boring-buffer-regexp-list "\\*scratch\\*")
  (add-to-list 'helm-boring-buffer-regexp-list "\\*Messages\\*")
  (add-to-list 'helm-boring-buffer-regexp-list (rx "magit-"))
  (add-to-list 'helm-boring-buffer-regexp-list (rx "*Flycheck"))

  (when (eq system-type 'windows-nt)
    (setq helm-locate-command "es %s -sort run-count %s")
    (defun helm-es-hook ()
      (when (and (equal (assoc-default 'name (helm-get-current-source)) "Locate")
                 (string-match "\\`es" helm-locate-command))
        (mapc (lambda (file)
                (call-process "es" nil nil nil
                              "-inc-run-count" (convert-standard-filename file)))
              (helm-marked-candidates))))
    (add-hook 'helm-find-many-files-after-hook 'helm-es-hook)))

(use-package helm-config)

(use-package helm-descbinds
  :defer t
  :bind ("C-h b" . helm-descbinds)
  :init
  (fset 'describe-bindings 'helm-descbinds))

(use-package helm-swoop
  :defer t
  :bind (("C-x c s" . helm-swoop)))

(use-package langtool
  :config
  (cond ((eq system-type 'gnu/linux)
         (setq langtool-bin "/usr/bin/languagetool"))
        ((eq system-type 'windows-nt)
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
  ;; :init
  ;; (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  ;;   (add-hook hook #'whitespace-mode))
  ;; (add-hook 'before-save-hook #'whitespace-cleanup)
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
  (add-to-list 'ibuffer-never-show-predicates "^\\*helm")
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-expert t)
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Dired" (mode . dired-mode))
           ("Planner"
            (or (filename . "\\(gtd\\|refile\\|reading\\|mobile\\|someday\\|archive\*\\).org")
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
                " " filename)))
  (defun my-ibuffer-mode-hook ()
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default")
    (hl-line-mode 1))
  (add-hook 'ibuffer-mode-hook #'my-ibuffer-mode-hook))

(use-package olivetti
  :commands olivetti-mode
  :bind ("C-c i o" . olivetti-mode))

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

(use-package smartparens-config
  :disabled t
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
  (add-to-list 'recentf-exclude "AppData/Local/Temp")
  (setq recentf-max-saved-items 60))

(use-package nginx-mode
  :init
  (add-to-list 'auto-mode-alist '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))


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
         (load-theme 'zenburn t)
         (let ((custom--inhibit-theme-enable nil))
           (custom-theme-set-faces
            'zenburn
            '(whitespace-tab ((t (:foreground "gray40" :background "#424242"))))
            '(ivy-current-match ((t (:background "#4f4f4f" :underline nil))))
            ))))

      ((eq system-type 'windows-nt)
       (set-face-attribute 'mode-line nil :box nil)
       (add-to-list 'default-frame-alist '(font . "Meslo LG S 11"))
       (setq inhibit-compacting-font-caches t)
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

(add-hook 'emacs-startup-hook #'emacs-min t)
;; (when (eq system-type 'windows-nt)
;;   (add-hook 'emacs-startup-hook #'emacs-maximize t))
(bind-key "C-<f12>" #'emacs-toggle-size)

;;; init.el ends here
