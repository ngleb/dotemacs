(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(setq package-pinned-packages
      '((async . "melpa-stable")
        (company . "melpa-stable")
        (dash . "melpa-stable")
        (helm . "melpa-stable")
        (helm-core . "melpa-stable")
        (ledger-mode . "melpa-stable")
        (markdown-mode . "melpa-stable")
        (flyspell-popup . "melpa-stable")

        (fill-column-indicator . "melpa")
        (deft . "melpa")
        (dired+ . "melpa")
        (olivetti . "melpa")
        (org-plus-contrib . "org")
        (smooth-scrolling . "melpa")
        (use-package . "melpa")))

(when (equal (system-name) "lenovo")
  (add-to-list 'package-pinned-packages '(zenburn-theme . "melpa-stable"))

  ;; ESS
  (add-to-list 'package-pinned-packages '(ess . "melpa-stable"))
  (add-to-list 'package-pinned-packages '(julia-mode . "melpa-stable"))

  ;; ;; elpy
  ;; (add-to-list 'package-pinned-packages '(find-file-in-project . "melpa-stable"))
  ;; (add-to-list 'package-pinned-packages '(highlight-indentation . "melpa-stable"))
  ;; (add-to-list 'package-pinned-packages '(pyvenv . "melpa-stable"))
  ;; (add-to-list 'package-pinned-packages '(elpy . "melpa-stable"))
  ;; (add-to-list 'package-pinned-packages '(yasnippet . "melpa-stable"))

  ;; company-jedi
  (add-to-list 'package-pinned-packages '(cl-lib . "melpa-stable"))
  (add-to-list 'package-pinned-packages '(epc . "melpa-stable"))
  (add-to-list 'package-pinned-packages '(python-environment . "melpa-stable"))
  (add-to-list 'package-pinned-packages '(jedi-core . "melpa-stable"))
  (add-to-list 'package-pinned-packages '(company-jedi . "melpa-stable"))

  ;; magit
  (add-to-list 'package-pinned-packages '(magit . "melpa-stable"))
  (add-to-list 'package-pinned-packages '(magit-popup . "melpa-stable"))
  (add-to-list 'package-pinned-packages '(git-commit . "melpa-stable"))
  (add-to-list 'package-pinned-packages '(with-editor . "melpa-stable")))

(when (eq system-type 'windows-nt)
  (add-to-list 'package-pinned-packages '(w32-browser . "melpa")))

(package-initialize)
(setq package-contents-refreshed nil)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

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

;; python settings
(use-package python-mode
  :init
  (progn
    (setq-default python-indent 4)
    (when (executable-find "ipython2.7")
      (setq python-shell-interpreter "ipython2.7"
            python-shell-interpreter-args "-i"))))

;; (use-package elpy
;;   :config
;;   (progn
;;     (elpy-enable)
;;     (setq elpy-rpc-backend "jedi")
;;     (elpy-use-ipython "ipython2")))

;; (use-package fill-column-indicator
;;   :init
;;   (setq-default fci-rule-column 80)
;;   (add-hook 'prog-mode-hook 'turn-on-fci-mode))

(use-package company
  :diminish company-mode
  :init
  (progn
    (setq company-idle-delay 0.5)
    (setq company-tooltip-limit 10)
    (setq company-minimum-prefix-length 2)
    (setq company-tooltip-flip-when-above t))
  :config
  (progn
    (bind-key "C-<tab>" 'company-complete)
    (add-hook 'prog-mode-hook 'global-company-mode)))

(use-package company-jedi
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package magit
  :init
  (bind-key "C-c m" 'magit-status))

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
  :config
  (progn
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

    (setq org-modules '(org-bbdb
                        org-habit))

    (cond ((eq system-type 'gnu/linux)
           (setq org-directory "~/my/org"))
          ((eq system-type 'windows-nt)
           (setq org-directory "C:/Users/nga/Documents/org")))

    (setq org-agenda-files
          (list (concat org-directory "/gtd.org")
                (concat org-directory "/someday.org")))

    ;; (setq org-archive-location
    ;;       (concat org-directory "/archive.org::datetree/"))
    ;; (setq org-archive-save-context-info nil)

    (setq org-capture-templates
          '(("x" "New inbound entry"
             entry
             (file+headline (concat org-directory "/gtd.org") "Inbox")
             "* %?\n%U\n")
            ("t" "New TODO entry"
             entry
             (file+headline (concat org-directory "/gtd.org") "Actions")
             "* TODO %?\n%U\n")))

    (define-key global-map "\C-cx"
      (lambda () (interactive) (org-capture nil "x")))
    (define-key global-map "\C-ct"
      (lambda () (interactive) (org-capture nil "t")))

    (setq org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9)))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-fontify-emphasized-text nil)
    (setq org-export-latex-tables-centered nil)

    (setq org-drawers '(("PROPERTIES" "LOGBOOK")))

    (setq org-tag-alist '(("@home" . ?h)
                          ("@laptop" . ?l)
                          ("@anna" . ?a)
                          ("@review" . ?r)
                          (:newline . nil)
                          ("@city" . ?c)
                          ("@buy" . ?b)
                          ("@project" . ?p)
                          ("@office" .?o)))

    (setq org-todo-keywords
          '((sequence "TODO(t)" "WAITING(w@/!)" "IN PROGRESS(p@/!)"
                      "|" "DONE(d!)" "CANCELED(c@)")))

    (setq org-agenda-custom-commands
          '(("h" "Agenda and Home-related tasks"
             ((agenda ""
                      ((org-agenda-span 'day)
                       (org-agenda-use-time-grid nil)
                       (org-agenda-prefix-format "%t")
                       (org-agenda-todo-keyword-format "")))
              (tags-todo "@home|@anna|@laptop|@review|@city|@buy"
                         ((org-agenda-overriding-header "\nNext actions for home")
                          (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                      'scheduled
                                                      'deadline
                                                      'timestamp))
                          (org-agenda-sorting-strategy '(tag-up))
                          (org-agenda-prefix-format "\t")))
              (todo "WAITING"
                    ((org-agenda-overriding-header "\nWaiting")
                     (org-agenda-prefix-format ""))))
             ((org-agenda-compact-blocks t)))

            ("o" "Agenda and Office-related tasks"
             ((agenda ""
                      ((org-agenda-span 'day)
                       (org-agenda-use-time-grid nil)
                       (org-agenda-prefix-format "\t%t")
                       (org-agenda-todo-keyword-format "")))
              (tags-todo "@office"
                         ((org-agenda-overriding-header "\nNext actions for office")
                          (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                      'scheduled
                                                      'deadline
                                                      'timestamp))
                          (org-agenda-sorting-strategy '(tag-up))
                          (org-agenda-prefix-format "\t")
                          ))
              (tags-todo "@review|@laptop|@anna"
                         ((org-agenda-overriding-header "\nOther related tasks")
                          (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                      'scheduled
                                                      'deadline
                                                      'timestamp))
                          (org-agenda-sorting-strategy '(tag-up))
                          (org-agenda-prefix-format "\t")
                          ))
              (todo "WAITING"
                    ((org-agenda-overriding-header "\nWaiting")
                     (org-agenda-prefix-format "\t"))))
             ((org-agenda-compact-blocks t)))))

    (eval-after-load "org" '(require 'ox-md nil t))
    (require 'ox-latex)

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
                   org-beamer-sectioning))))

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
  ;; (setq deft-directory "~/Reference")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase)))
  (setq deft-text-mode 'org-mode))

(use-package helm
  :bind
  (("M-x" . undefined)
   ("M-x" . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files))
  :init
  (progn
    (require 'helm-config)
    (helm-mode 1)
    (helm-autoresize-mode 1)))

(use-package smooth-scrolling
  :init
  (progn
    (setq smooth-scroll-margin 5))
  :config
  (progn
    (smooth-scrolling-mode 1)))

(use-package ibuffer
  :commands ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (progn
    (use-package ibuf-ext)
    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-saved-filter-groups
          '(("default"
             ("planner"
              (or (filename . "gtd.org")
                  (filename . "someday.org")
                  (filename . "journal.org")
                  (filename . "inbox.org")
                  (mode . org-agenda-mode)
                  (name . "^\\*Calendar\\*$")
                  (name . "^diary$")))
             ("dired" (mode . dired-mode))
             ("text"
              (or (name . "\\.\\(tex\\|bib\\|csv\\)")
                  (mode . org-mode)
                  (mode . markdown-mode)
                  (mode . text-mode)))
             ("emacs"
              (or (name . "^\\*scratch\\*$")
                  (name . "^\\*Messages\\*$")
                  (name . "^\\*Help\\*$")
                  (name . "^\\*info\\*$")
                  (name . "\*.*\*")))
             )))
    (add-hook 'ibuffer-mode-hook
              '(lambda ()
                 (hl-line-mode 1)
                 (ibuffer-auto-mode 1)
                 (ibuffer-switch-to-saved-filter-groups "default")))
    (add-to-list 'ibuffer-never-show-predicates "^\\*helm")))

(use-package ledger-mode
  :mode "\\.ledger\\'")

(use-package olivetti
  :bind
  ("<f6>" . olivetti-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :config
  (progn
    (add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)
    (add-hook 'markdown-mode-hook 'turn-on-olivetti-mode)))

(use-package flyspell
  :bind
  ("<f8>" . flyspell-buffer)
  ("<f7>" . ispell-word)
  ;; :init
  ;; (progn
  ;;   (dolist (hook '(text-mode-hook org-mode-hook))
  ;;     (add-hook hook (lambda () (flyspell-mode 1))))
  ;;   (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config
  (progn
    (when (eq system-type 'windows-nt)
      (setq ispell-program-name
            "C:/Users/nga/Applications/Hunspell/bin/hunspell.exe"))
    (setq ispell-really-hunspell t)
    (setq ispell-dictionary "english")

    (add-to-list 'ispell-local-dictionary-alist
                 '("english"
                   "[[:alpha:]]"
                   "[^[:alpha:]]"
                   "[']"
                   t
                   ("-d" "en_US")
                   nil
                   utf-8))
    (add-to-list 'ispell-local-dictionary-alist
                 '("russian"
                   "[[:alpha:]]"
                   "[^[:alpha:]]"
                   "[']"
                   t
                   ("-d" "ru")
                   nil
                   utf-8))
    (global-set-key
     [f3]
     (lambda ()
       (interactive)
       (ispell-change-dictionary "russian")))
    (global-set-key
     [f4]
     (lambda ()
       (interactive)
       (ispell-change-dictionary "english")))

    (setq ispell-hunspell-dictionary-alist
          ispell-local-dictionary-alist)))

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

;; (setq tex-compile-commands '(("xelatex %r")))
;; (setq tex-command "xelatex")
;; (setq-default TeX-engine 'xelatex)

(cond ((eq system-type 'gnu/linux)
       ;; Frame size, position, font
       (add-to-list 'default-frame-alist '(width . 140))
       (add-to-list 'default-frame-alist '(height . 50))
       (add-to-list 'default-frame-alist '(top . 100))
       (add-to-list 'default-frame-alist '(left . 300))
       (add-to-list 'default-frame-alist '(font . "Meslo LG M 11"))
       (use-package zenburn-theme
         :config
         (load-theme 'zenburn t)))

      ;; Windows specific setup for office
      ((eq system-type 'windows-nt)

       ;; Window size & position; font settings
       (add-to-list 'default-frame-alist '(width  . 120))
       (add-to-list 'default-frame-alist '(height . 40))
       (add-to-list 'default-frame-alist '(top . 90))
       (add-to-list 'default-frame-alist '(left . 290))
       (add-to-list 'default-frame-alist '(font . "Meslo LG S 11"))

       ;; default directory
       (setq default-directory "C:/Users/nga/Documents/")

       ;; exec-path for Windows 7 installation at office
       (add-to-list 'exec-path "C:/Users/nga/Applications/bin")

       ;; hotkeys for quick opening working dirs
       (global-set-key (kbd "S-<f1>")
                       (lambda () (interactive) (dired "C:/Users/nga/Documents")))
       (global-set-key (kbd "S-<f2>")
                       (lambda () (interactive) (dired "C:/Users/nga/Downloads")))

       (use-package w32-browser)))

(setq custom-file "~/.emacs.d/custom.el")
(load-file "~/.emacs.d/personal.el")

;;; init.el ends here
