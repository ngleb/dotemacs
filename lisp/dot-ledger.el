;;; ledger config

(use-package ledger-mode
  :mode "\\.ledger\\'"
  :config
  (setq ledger-report-links-in-register nil)
  (setq ledger-report-use-header-line t)
  (setq ledger-reports
        (quote
         (("bal" "ledger -f %(ledger-file) --strict bal")
          ("reg" "ledger -f %(ledger-file) --strict reg")
          ("payee" "ledger -f %(ledger-file) --strict reg @%(payee)")
          ("account" "ledger -f %(ledger-file) --strict reg %(account)")
          ("bal-assets" "ledger -f %(ledger-file) --strict bal assets and not Foreign and not Reim")
          ("bal-assets-full" "ledger -f %(ledger-file) --strict bal assets")
          ("exp-this-month" "ledger -f %(ledger-file) --strict -p \"this month\" bal expenses")
          ("exp-last-month" "ledger -f %(ledger-file) --strict -p \"last month\" bal expenses")
          ("exp-this-week" "ledger -f %(ledger-file) --strict -p \"this week\" --start-of-week=1 bal expenses")
          ("exp-last-week" "ledger -f %(ledger-file) --strict -p \"last week\" --start-of-week=1 bal expenses")
          ("bu-this-month" "ledger -f %(ledger-file) --strict -p \"this month\" bal --unbudgeted ^exp")
          ("bb-this-month" "ledger -f %(ledger-file) --strict -p \"this month\" bal --budget ^exp --invert")
          ("bu-last-month" "ledger -f %(ledger-file) --strict -p \"last month\" bal --unbudgeted ^exp")
          ("bb-last-month" "ledger -f %(ledger-file) --strict -p \"last month\" bal --budget ^exp --invert")
          ("reg-01" "ledger -f %(ledger-file) --strict -d \"d>=[last 45 days]\" reg assets:cash")
          ("reg-02" "ledger -f %(ledger-file) --strict -d \"d>=[last 45 days]\" reg assets:checking:a")
          ("reg-03" "ledger -f %(ledger-file) --strict -d \"d>=[last 45 days]\" reg assets:checking:g")
          ("reg-04" "ledger -f %(ledger-file) --strict -d \"d>=[last 45 days]\" reg assets:checking:v"))))

  (when (string= (system-name) "ACER")
    (bind-key "<f8>" #'my-ledger-report/body ledger-mode-map)
    (bind-key "<f9>" #'ledger-mode-clean-buffer ledger-mode-map))

  (bind-key "M-<f8>" #'my-ledger-report/body ledger-mode-map)
  (bind-key "n" #'next-line ledger-report-mode-map)
  (bind-key "p" #'previous-line ledger-report-mode-map)
  (add-hook 'ledger-report-mode-hook (lambda () (hl-line-mode 1)))

  (add-hook 'ledger-mode-hook 'flycheck-mode)
  (use-package flycheck-ledger)

  (defun gn/ledger-report (&optional arg split)
    "Open ledger-report"
    (interactive "P")
    (ledger-report arg nil)
    (when (not split)
      (delete-other-windows)))

  (defhydra my-ledger-report (nil nil :foreign-keys nil :hint nil :exit t)
    "
Balance:            Budget:          Unbudgeted:
_1_: Assets         _u_: This month  _o_: This month
_2_: Assets (full)  _i_: Last month  _p_: Last month

Expenses:           Register:
_3_: This month     _c_: Cash
_4_: Last month     _a_: A
_5_: This week      _g_: G
_6_: Last week      _v_: V

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

    ("c" (gn/ledger-report "reg-01"))
    ("a" (gn/ledger-report "reg-02"))
    ("g" (gn/ledger-report "reg-03"))
    ("v" (gn/ledger-report "reg-04"))))

(provide 'dot-ledger)
