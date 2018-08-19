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

  (setq ledger-reports
        (quote
         (("bal" "%(binary) -f %(ledger-file) --strict -w bal")
          ("reg" "%(binary) -f %(ledger-file) --strict -w reg")
          ("payee" "%(binary) -f %(ledger-file) --strict -w reg @%(payee)")
          ("account" "%(binary) -f %(ledger-file) --strict -w reg %(account)")
          ("account45" "%(binary) -f %(ledger-file) --strict -w -d \"d>=[last 45 days]\" reg %(account)")
          ("bal-assets" "%(binary) -f %(ledger-file) --strict -w bal assets and not Foreign and not Reim")
          ("bal-assets-full" "%(binary) -f %(ledger-file) --strict -w bal assets")
          ("exp-this-month" "%(binary) -f %(ledger-file) --aux-date --strict -w -p \"this month\" bal expenses")
          ("exp-last-month" "%(binary) -f %(ledger-file) --aux-date --strict -w -p \"last month\" bal expenses")
          ("exp-this-week" "%(binary) -f %(ledger-file) --strict -w -p \"this week\" --start-of-week=1 bal expenses")
          ("exp-last-week" "%(binary) -f %(ledger-file) --strict -w -p \"last week\" --start-of-week=1 bal expenses")
          ("bu-this-month" "%(binary) -f %(ledger-file) --aux-date --strict -w -p \"this month\" bal --unbudgeted ^exp")
          ("bb-this-month" "%(binary) -f %(ledger-file) --aux-date --strict -w -p \"this month\" bal --budget ^exp --invert")
          ("bu-last-month" "%(binary) -f %(ledger-file) --aux-date --strict -w -p \"last month\" bal --unbudgeted ^exp")
          ("bb-last-month" "%(binary) -f %(ledger-file) --aux-date --strict -w -p \"last month\" bal --budget ^exp --invert"))))

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
Balance:            Budget:          Unbudgeted:
_1_: Assets         _u_: This month  _o_: This month
_2_: Assets (full)  _i_: Last month  _p_: Last month

Expenses:           Register:
_3_: This month     _r_: Register report for account
_4_: Last month
_5_: This week
_6_: Last week

_q_ quit"
    ("q" nil)
    ("1" (gn/ledger-report "bal-assets"))
    ("2" (gn/ledger-report "bal-assets-full"))

    ("3" (gn/ledger-report "exp-this-month"))
    ("4" (gn/ledger-report "exp-last-month"))
    ("5" (gn/ledger-report "exp-this-week"))
    ("6" (gn/ledger-report "exp-last-week"))

    ("u" (gn/ledger-report "bb-this-month"))
    ("i" (gn/ledger-report "bb-last-month"))

    ("o" (gn/ledger-report "bu-this-month"))
    ("p" (gn/ledger-report "bu-last-month"))

    ("r" (gn/ledger-report "account45"))))

(use-package flycheck-ledger
  :after flycheck)

;;; init-ledger.el ends here
