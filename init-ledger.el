(use-package ledger-mode
  :mode "\\.ledger\\'"
  :bind (:map ledger-mode-map
         ("<f7>"   . gn/quick-calc)
         ("C-<f7>" . quick-calc)
         ("<f8>"   . my-ledger-report/body)
         ("<f9>"   . ledger-mode-clean-buffer)
         :map ledger-report-mode-map
         ("n"      . next-line)
         ("p"      . previous-line))

  :config
  (setq ledger-report-links-in-register nil)
  (setq ledger-report-use-header-line t)
  (setq ledger-report-use-strict t)

  (setq ledger-reports
        (quote
         (("bal" "%(binary) -f %(ledger-file) --wide balance")
          ("reg" "%(binary) -f %(ledger-file) --wide register")
          ("payee" "%(binary) -f %(ledger-file) --wide register @%(payee)")
          ("account" "%(binary) -f %(ledger-file) --wide register %(account)")
          ("account-last-45days" "%(binary) -f %(ledger-file) --wide -d \"d>=[last 45 days]\" register %(account)")
          ("assets-short" "%(binary) -f %(ledger-file) --wide balance ^assets and not Foreign and not Reim")
          ("assets-full" "%(binary) -f %(ledger-file) --wide balance ^assets")
          ("expenses-monthly" "%(binary) -f %(ledger-file) --period %(month) --aux-date --wide balance ^expenses")
          ("budget-monthly" "%(binary) -f %(ledger-file) --period %(month) --aux-date --wide --budget --invert bal ^expenses")
          ("unbudgeted-monthly" "%(binary) -f %(ledger-file) --period %(month) --aux-date --wide --unbudgeted balance ^expenses"))))

  (defun my-ledger-mode-hook ()
    (flycheck-mode 1)
    (company-mode 1)
    (setq pcomplete-ignore-case t)
    (setq completion-ignore-case t))
  (add-hook 'ledger-mode-hook 'my-ledger-mode-hook)

  (defun my-ledger-report-mode-hook ()
    (hl-line-mode 1))
  (add-hook 'ledger-report-mode-hook 'my-ledger-report-mode-hook)

  (defun my-center-buffer (&rest args)
    (recenter))
  (advice-add 'ledger-add-transaction :after 'my-center-buffer)

  (defun gn/quick-calc ()
    "Run `quick-calc' interactively with C-u prefix. The result will be inserted at point."
    (interactive)
    (setq current-prefix-arg '(4)) ;; C-u
    (call-interactively 'quick-calc))

  (defun gn/ledger-report (&optional arg)
    "Open ledger-report with where `arg' is report name."
    (interactive "P")
    (ledger-report arg nil)
    (delete-other-windows))

  (defhydra my-ledger-report (nil nil :foreign-keys nil :hint nil :exit t)
    "
Balance:               Reports                  Budget:
_a_: Assets (short)    _e_: Monthly expenses    _b_: Monthly budget
_f_: Assets (full)     _r_: Account register    _u_: Monthly unbudgeted
_1_: Balance

_q_ quit"
    ("q" nil)
    ("a" (gn/ledger-report "assets-short"))
    ("f" (gn/ledger-report "assets-full"))

    ("e" (gn/ledger-report "expenses-monthly"))
    ("r" (gn/ledger-report "account-last-45days"))

    ("b" (gn/ledger-report "budget-monthly"))
    ("u" (gn/ledger-report "unbudgeted-monthly"))

    ("1" (gn/ledger-report "bal"))))

(use-package flycheck-ledger
  :after flycheck)

;;; init-ledger.el ends here
