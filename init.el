(setq gc-cons-threshold 100000000)

(require 'package)
(require 'server)

(or (eq (server-running-p) t)
    (server-start))

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
        (flx . "melpa")
        (popup . "melpa-stable")
        (flyspell-popup . "melpa-stable")

        (company . "melpa-stable")
        (deft . "melpa-stable")
        (ledger-mode . "melpa")
        (markdown-mode . "melpa-stable")
        (olivetti . "melpa-stable")
        (nlinum . "gnu")
        (langtool . "melpa")

        (bind-key . "melpa")
        (diminish . "melpa")
        (use-package . "melpa")
        (hydra . "melpa")

        (dired+ . "melpa")
        (smooth-scrolling . "melpa")
        (zenburn-theme . "melpa")
        (org-plus-contrib . "org")

        ;; magit
        (magit . "melpa-stable")
        (magit-popup . "melpa-stable")
        (async . "melpa-stable")
        (git-commit . "melpa-stable")
        (with-editor . "melpa-stable")

        (flycheck . "melpa-stable")
        (flycheck-ledger . "melpa")

        (elpy . "melpa-stable")
        (find-file-in-project . "melpa-stable")
        (highlight-indentation . "melpa-stable")
        (pyvenv . "melpa-stable")
        (w32-browser . "melpa")
        (leuven-theme . "melpa")))

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

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(defvar gn/default-dir-tmp
  (cond ((eq system-type 'gnu/linux) (expand-file-name "~"))
        ((eq system-type 'windows-nt) (expand-file-name user-login-name "C:/Users"))))
(setq gn/default-dir (file-name-as-directory gn/default-dir-tmp))

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

;; remove warning
;; ad-handle-definition: `tramp-read-passwd' got redefined
(setq ad-redefinition-action 'accept)

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
(global-auto-revert-mode 1)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(bind-key "M-n" (kbd "C-u 1 C-v"))
(bind-key "M-p" (kbd "C-u 1 M-v"))
(bind-key "M-z" #'zap-up-to-char)
(bind-key "M-/" #'hippie-expand)
(bind-key "<f5>" #'toggle-truncate-lines)
(bind-key "C-c e" (lambda () (interactive) (find-file user-init-file)))

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

(eval-when-compile
  (defvar emacs-min-height)
  (defvar emacs-min-width))

(defvar display-name
  (let ((height (display-pixel-height)))
    (cond ((= height 1200) 'lenovo-vga)
          ((= height 768) 'lenovo)
          ((= height 1080) 'office))))

(defvar emacs-min-top 20)
(defvar emacs-min-left
  (cond ((eq display-name 'office) 200)
        ((eq display-name 'lenovo) 100)
        ((eq display-name 'lenovo-vga) 220)
        (t 220)))
(defvar emacs-min-height
  (cond ((eq display-name 'office) 52)
        ((eq display-name 'lenovo) 40)
        ((eq display-name 'lenovo-vga) 52)
        (t 40)))
(defvar emacs-min-width
  (cond ((eq display-name 'office) 160)
        ((eq display-name 'lenovo) 140)
        ((eq display-name 'lenovo-vga) 160)
        (t 100)))

(let ((frame-alist
       (list (cons 'top    emacs-min-top)
             (cons 'left   emacs-min-left)
             (cons 'height emacs-min-height)
             (cons 'width  emacs-min-width))))
  (setq initial-frame-alist frame-alist))

(defun emacs-min ()
  (interactive)

  (set-frame-parameter (selected-frame) 'fullscreen nil)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)

  (set-frame-parameter (selected-frame) 'top emacs-min-top)
  (set-frame-parameter (selected-frame) 'left emacs-min-left)
  (set-frame-parameter (selected-frame) 'height emacs-min-height)
  (set-frame-parameter (selected-frame) 'width emacs-min-width))

(if window-system
    (add-hook 'after-init-hook 'emacs-min))

(defun emacs-max ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))

(defun emacs-toggle-size ()
  (interactive)
  (if (> (cdr (assq 'width (frame-parameters))) 161)
      (emacs-min)
    (emacs-max)))

(use-package elisp-mode
  :config
  (use-package eldoc
    :config
    (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)))

(use-package flx)

(use-package swiper
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-do-completion-in-region nil)
  (ivy-mode 1)
  (bind-key "C-s" #'swiper)
  (bind-key "M-x" #'counsel-M-x)
  (bind-key "C-x C-f" #'counsel-find-file)
  (bind-key "C-c C-r" #'ivy-resume)
  (bind-key "C-c j" #'counsel-imenu)
  (bind-key "C-x l" #'counsel-locate)
  (bind-key "C-r" #'counsel-expression-history read-expression-map))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package company
  :commands company-mode
  :bind
  (("C-<tab>" . company-complete))
  :init
  (setq company-require-match nil)
  (setq company-idle-delay 0.5)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-flip-when-above t)
  :config
  (add-hook 'prog-mode-hook 'global-company-mode))

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
  (setq elpy-rpc-python-command "python3.5")
  (setq elpy-rpc-backend "jedi")
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i")
    (elpy-use-ipython)))

(use-package langtool
  :config
  (defvar gn/langtool-path
    (cond ((eq system-type 'windows-nt) (expand-file-name "Applications/LanguageTool/languagetool-commandline.jar" gn/default-dir))
          ((eq system-type 'gnu/linux) (expand-file-name "my/bin/langtool/languagetool-commandline.jar" gn/default-dir))))
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
  (setq whitespace-style '(face tabs tab-mark trailing))

  (custom-set-faces
   '(whitespace-tab ((t (:foreground "gray40" :background "#424242"))))))

(use-package deft
  :bind
  (("<f9>" . deft)
   ("C-c C-g" . deft-find-file))
  :config
  (setq deft-default-extension "org")
  (setq deft-extensions '("org" "txt" "text" "md" "text" "markdown"))
  (setq deft-directory (expand-file-name "doc/reference/" gn/default-dir))
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

(use-package ibuffer
  :commands ibuffer
  :bind
  ("C-x C-b" . ibuffer)
  :config
  (use-package ibuf-ext)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-expert t)
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Dired" (mode . dired-mode))
           ("Text"
            (or (name . "\\.\\(tex\\|bib\\|csv\\)")
                (mode . org-mode)
                (mode . markdown-mode)
                (mode . text-mode)
                (mode . ledger-mode)))
           ("Planner"
            (or (filename . "todo.org")
                (filename . "refile.org")
                (filename . "someday.org")
                (filename . "journal.org")
                (filename . "journal.org.gpg")
                (filename . "mobile.org")
                (filename . "archive\*.org")
                (mode . org-agenda-mode)
                (name . "^\\*Calendar\\*$")
                (name . "^diary$")
                (name . "^org$")))
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
  (add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook))

(use-package olivetti
  :bind
  ("<f6>" . olivetti-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown.exe")
  (add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'markdown-mode-hook 'turn-on-olivetti-mode))

(use-package ispell
  :bind
  ("<f7>" . ispell-word)
  :commands
  (ispell-word)
  :config
  (add-to-list 'ispell-local-dictionary-alist
               '("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8))
  (add-to-list 'ispell-local-dictionary-alist
               '("russian" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "ru") nil koi8-r))
  (setq ispell-dictionary "english")
  (setq ispell-silently-savep t)
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))

  (defun gn-toggle-ispell-dictionary ()
    "Switch russian and english dictionaries."
    (interactive)
    (let* ((dict ispell-current-dictionary)
           (new (if (string= dict "russian") "english"
                  "russian")))
      (ispell-change-dictionary new)
      (message "Switched dictionary from %s to %s" dict new)))

  (bind-key "C-c i d" #'gn-toggle-ispell-dictionary)

  (use-package flyspell
    :bind
    (("C-c i b" . flyspell-buffer)
     ("C-c i m" . flyspell-mode))
    ;; :init
    ;; (progn
    ;;   (dolist (hook '(text-mode-hook org-mode-hook))
    ;;     (add-hook hook (lambda () (flyspell-mode 1))))
    ;;   (add-hook 'prog-mode-hook 'flyspell-prog-mode))
    :config
    ;; Flyspell signals an error if there is no spell-checking tool is
    ;; installed. We can advice `turn-on-flyspell' and `flyspell-prog-mode'
    ;; to try to enable flyspell only if a spell-checking tool is available.
    (defun modi/ispell-not-avail-p (&rest args)
      "Return `nil' if `ispell-program-name' is available; `t' otherwise."
      (not (executable-find ispell-program-name)))
    (advice-add 'turn-on-flyspell   :before-until #'modi/ispell-not-avail-p)
    (advice-add 'flyspell-prog-mode :before-until #'modi/ispell-not-avail-p)

    (defun flyspell-check-next-highlighted-word ()
      "Custom function to spell check next highlighted word"
      (interactive)
      (flyspell-goto-next-error)
      (ispell-word))
    (bind-key "C-;" #'flyspell-popup-correct flyspell-mode-map)
    (bind-key "M-<f8>" #'flyspell-check-next-highlighted-word)))

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
       (add-to-list 'default-frame-alist '(font . "Hack 11"))

       (use-package leuven-theme
         :config
         (setq leuven-scale-org-agenda-structure nil)
         (setq leuven-scale-outline-headlines nil)
         (load-theme 'leuven t))

       (setq default-directory gn/default-dir)

       (use-package w32-browser)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
(load-file "~/.emacs.d/personal.el")

(use-package dot-org)

;;; init.el ends here
