(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(setq package-pinned-packages
      '((dash . "melpa-stable")

        (ivy . "melpa-stable")
        (swiper . "melpa-stable")
        (counsel . "melpa-stable")

        (popup . "melpa-stable")
        (flyspell-popup . "melpa-stable")

        (company . "melpa-stable")
        (deft . "melpa-stable")
        (ledger-mode . "melpa-stable")
        (markdown-mode . "melpa-stable")
        (olivetti . "melpa-stable")
        (yasnippet . "melpa-stable")
        (nlinum . "gnu")

        (dired+ . "melpa")
        (smooth-scrolling . "melpa")
        (use-package . "melpa")
        (zenburn-theme . "melpa")
        (org-plus-contrib . "org")

        ;; magit
        (magit . "melpa-stable")
        (magit-popup . "melpa-stable")
        (async . "melpa-stable")
        (git-commit . "melpa-stable")
        (with-editor . "melpa-stable")))

(when (equal (system-name) "lenovo")
  ;; ESS
  (add-to-list 'package-pinned-packages '(ess . "melpa-stable"))
  (add-to-list 'package-pinned-packages '(julia-mode . "melpa-stable")))

(when (eq system-type 'windows-nt)
  (add-to-list 'package-pinned-packages '(w32-browser . "melpa")))

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
(require 'diminish)
(require 'bind-key)

;; open init.el via hotkey
(global-set-key (kbd "C-c e")
                (lambda ()
                  (interactive)
                  (find-file user-init-file)))

;; window elements
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
(setq visible-bell t)
(setq x-underline-at-descent-line t)
;;(add-hook 'prog-mode-hook 'nlinum-mode)

;; Truncate lines
(setq-default truncate-lines t)
(setq-default word-wrap t)
(define-key global-map [f5] 'toggle-truncate-lines)

;; Tab & indent setup
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

;; Startup tweaks
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Other tweaks
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq sentence-end-double-space nil)

;; MULE & encoding setup
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq default-input-method "russian-computer")

;; Stop creating backub and autosave files
(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #autosave# files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; remove warning
;; ad-handle-definition: `tramp-read-passwd' got redefined
(setq ad-redefinition-action 'accept)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)
;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

(use-package swiper
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy) ;; TODO
  (setq ivy-count-format "(%d/%d) ")

  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  ;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  ;; (global-set-key (kbd "<f1> l") 'counsel-load-library)
  ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package company
  :diminish company-mode
  :init
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-flip-when-above t)
  :config
  (bind-key "C-<tab>" 'company-complete)
  (add-hook 'prog-mode-hook 'global-company-mode))

(use-package flycheck
  :defer t)

(setq elfeed-feeds
      '("https://www.smashingmagazine.com/feed/"))

(use-package magit
  :bind
  (("C-c m" . magit-status)))

(use-package whitespace
  :diminish whitespace-mode
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs tab-mark trailing)))

(use-package org
  :mode (("\\.org$" . org-mode))
  :bind
  (("C-c a" . org-agenda)
   ("C-c l" . org-store-link)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture))
  :commands
  (org
   org-capture
   org-mode
   org-store-link)
  :config
  (setq org-modules '(org-habit))
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'org-mode-hook
            (lambda ()
              (visual-line-mode 1)))
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (visual-line-mode -1)
              (toggle-truncate-lines 1)))
  (setq org-startup-indented t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-use-fast-todo-selection t)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (setq org-archive-save-context-info nil)
  (setq org-habit-show-habits-only-for-today nil)
  (cond ((eq system-type 'gnu/linux)
         (setq org-directory "~/my/org"))
        ((eq system-type 'windows-nt)
         (setq org-directory "C:/Users/nga/Documents/org")))

  (setq org-agenda-files
        (list (concat org-directory "/todo.org")
              (concat org-directory "/someday.org")))

  (setq org-capture-templates
        '(("x" "New inbound entry"
           entry
           (file+headline (concat org-directory "/todo.org") "Inbox")
           "* %?\n%U\n")
          ("t" "New TODO entry"
           entry
           (file+headline (concat org-directory "/todo.org") "Actions")
           "* TODO %?\n%U\n")
          ("w" "Log Work Task" entry
           (file+datetree (concat org-directory "/worklog.org"))
           "* TODO %^{Description}  %^g\n%?\n\nAdded: %U"
           :clock-in t
           :clock-keep t)))

  (define-key global-map "\C-cx"
    (lambda () (interactive) (org-capture nil "x")))
  (define-key global-map "\C-ct"
    (lambda () (interactive) (org-capture nil "t")))

  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-fontify-emphasized-text nil)

  (setq org-mobile-directory "~/ownCloud/MobileOrg")
  (setq org-mobile-inbox-for-pull "~/my/org/mobile.org")

  (setq org-drawers '(("PROPERTIES" "LOGBOOK")))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)"
                    "|" "DONE(d!)" "CANCELED(c@)")))

  ;; (setq org-latex-pdf-process
  ;;       '("xelatex -interaction nonstopmode -output-directory %o %f"
  ;;         "xelatex -interaction nonstopmode -output-directory %o %f"
  ;;         "xelatex -interaction nonstopmode -output-directory %o %f"))

  (add-to-list 'org-latex-classes
               '("mybeamer"
                 "\\documentclass[presentation,smaller,12pt]{beamer}
                  \\usepackage{polyglossia}
                  \\setmainlanguage{russian}
                  [NO-DEFAULT-PACKAGES]
                  [NO-PACKAGES]
                  [EXTRA]"
                 org-beamer-sectioning)))

(use-package deft
  :bind
  (("<f9>" . deft)
   ("C-x C-g" . deft-find-file))
  :config
  (setq deft-default-extension "org")
  (setq deft-extensions '("org" "txt" "text" "md" "text" "markdown"))
  (cond ((eq system-type 'gnu/linux)
         (setq deft-directory "~/Reference"))
        ((eq system-type 'windows-nt)
         (setq deft-directory "c:/Users/nga/Documents/Reference")))
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase)))
  (setq deft-text-mode 'org-mode))

(use-package smooth-scrolling
  :config
  (setq smooth-scroll-margin 5)
  (smooth-scrolling-mode 1))

(use-package ibuffer
  :commands ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (use-package ibuf-ext)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("planner"
            (or (filename . "todo.org")
                (filename . "someday.org")
                (filename . "journal.org")
                (filename . "journal.org.gpg")
                (filename . "mobile.org")
                (filename . "archive.org")
                (mode . org-agenda-mode)
                (name . "^\\*Calendar\\*$")
                (name . "^diary$")))
           ("text"
            (or (name . "\\.\\(tex\\|bib\\|csv\\)")
                (mode . org-mode)
                (mode . markdown-mode)
                (mode . text-mode)))
           ("dired" (mode . dired-mode))
           ("emacs"
            (or (name . "^\\*scratch\\*$")
                (name . "^\\*Messages\\*$")
                (name . "^\\*Help\\*$")
                (name . "^\\*info\\*$")
                (name . "\*.*\*"))))))
  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (hl-line-mode 1)
               (ibuffer-auto-mode 1)
               (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package ledger-mode
  :mode "\\.ledger\\'")

(use-package olivetti
  :bind
  ("<f6>" . olivetti-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :config
  (add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'markdown-mode-hook 'turn-on-olivetti-mode))

(use-package flyspell
  :bind
  (("<f8>" . flyspell-buffer)
   ("<f7>" . ispell-word))
  ;; auto spellchecking everywhere after startup
  ;; currently disabled because doesn't work well
  ;; poor performance
  ;; :init
  ;; (progn
  ;;   (dolist (hook '(text-mode-hook org-mode-hook))
  ;;     (add-hook hook (lambda () (flyspell-mode 1))))
  ;;   (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (when (eq system-type 'windows-nt)
    (setq ispell-program-name "hunspell.exe"))
  (setq ispell-really-hunspell t)

  (add-to-list 'ispell-local-dictionary-alist
               '("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8))
  (add-to-list 'ispell-local-dictionary-alist
               '("russian" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "ru") nil utf-8))

  (global-set-key [f3] (lambda ()
                         (interactive)
                         (ispell-change-dictionary "russian")))
  (global-set-key [f4] (lambda ()
                         (interactive)
                         (ispell-change-dictionary "english")))

  (setq ispell-dictionary "english")
  (setq ispell-hunspell-dictionary-alist
        ispell-local-dictionary-alist))

(use-package dired
 :config
 (use-package dired-x)
 (use-package dired+
   :config
   (setq diredp-hide-details-initially-flag nil)
   (diredp-toggle-find-file-reuse-dir 1))

 (add-hook 'dired-mode-hook '(lambda () (hl-line-mode 1)))
 (cond ((eq system-type 'gnu/linux)
        (setq dired-listing-switches
              "-aBhl --group-directories-first"))
       ((eq system-type 'windows-nt)
         (setq dired-listing-switches "-alh"))))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(cond ((eq system-type 'gnu/linux)
       (add-to-list 'default-frame-alist '(width . 160))
       (add-to-list 'default-frame-alist '(height . 50))
       (add-to-list 'default-frame-alist '(top . 90))
       (add-to-list 'default-frame-alist '(left . 240))
       (add-to-list 'default-frame-alist '(font . "Meslo LG M 11")))

      ((eq system-type 'windows-nt)
       (add-to-list 'default-frame-alist '(width  . 120))
       (add-to-list 'default-frame-alist '(height . 40))
       (add-to-list 'default-frame-alist '(top . 90))
       (add-to-list 'default-frame-alist '(left . 290))
       (add-to-list 'default-frame-alist '(font . "Meslo LG S 11"))

       (setq my-work-dir (concat "C:/Users/" user-login-name))
       (setq default-directory (file-name-as-directory my-work-dir))

       (add-to-list 'exec-path (concat default-directory "Applications/bin"))
       (add-to-list 'exec-path "c:/Program Files (x86)/GNU/GnuPG")
       (add-to-list 'exec-path "c:/Program Files (x86)/GNU/GnuPG/bin")
       (add-to-list 'exec-path "c:/Program Files (x86)/Git/bin")

       (global-set-key (kbd "S-<f1>")
                       (lambda ()
                         (interactive)
                         (dired (concat default-directory "Documents"))))
       (global-set-key (kbd "S-<f2>")
                       (lambda ()
                         (interactive)
                         (dired (concat default-directory "Downloads"))))

       (use-package w32-browser)))

(setq custom-file "~/.emacs.d/custom.el")
;;(load-file "~/.emacs.d/custom.el")
(load-file "~/.emacs.d/personal.el")

;;; init.el ends here
