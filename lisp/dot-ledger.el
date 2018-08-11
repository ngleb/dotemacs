;;; ledger config

(use-package ledger-mode
  :mode "\\.ledger\\'"
  :config
  (setq ledger-report-links-in-register nil)
  (setq ledger-report-use-header-line t)
  (setq ledger-reports
        (quote
         (("bal" "%(binary) -f %(ledger-file) --strict -w bal")
          ("reg" "%(binary) -f %(ledger-file) --strict -w reg")
          ("payee" "%(binary) -f %(ledger-file) --strict -w reg @%(payee)")
          ("account" "%(binary) -f %(ledger-file) --strict -w reg %(account)")
          ("bal-assets" "%(binary) -f %(ledger-file) --strict -w bal assets and not Foreign and not Reim")
          ("bal-assets-full" "%(binary) -f %(ledger-file) --strict -w bal assets")
          ("exp-this-month" "%(binary) -f %(ledger-file) --strict -w -p \"this month\" bal expenses")
          ("exp-last-month" "%(binary) -f %(ledger-file) --strict -w -p \"last month\" bal expenses")
          ("exp-this-week" "%(binary) -f %(ledger-file) --strict -w -p \"this week\" --start-of-week=1 bal expenses")
          ("exp-last-week" "%(binary) -f %(ledger-file) --strict -w -p \"last week\" --start-of-week=1 bal expenses")
          ("bu-this-month" "%(binary) -f %(ledger-file) --strict -w -p \"this month\" bal --unbudgeted ^exp")
          ("bb-this-month" "%(binary) -f %(ledger-file) --strict -w -p \"this month\" bal --budget ^exp --invert")
          ("bu-last-month" "%(binary) -f %(ledger-file) --strict -w -p \"last month\" bal --unbudgeted ^exp")
          ("bb-last-month" "%(binary) -f %(ledger-file) --strict -w -p \"last month\" bal --budget ^exp --invert")
          ("reg-01" "%(binary) -f %(ledger-file) --strict -w -d \"d>=[last 45 days]\" reg assets:cash")
          ("reg-02" "%(binary) -f %(ledger-file) --strict -w -d \"d>=[last 45 days]\" reg assets:checking:an")
          ("reg-03" "%(binary) -f %(ledger-file) --strict -w -d \"d>=[last 45 days]\" reg assets:checking:g")
          ("reg-04" "%(binary) -f %(ledger-file) --strict -w -d \"d>=[last 45 days]\" reg assets:checking:v")
          ("reg-05" "%(binary) -f %(ledger-file) --strict -w -d \"d>=[last 45 days]\" reg assets:checking:av")
          ("reg-06" "%(binary) -f %(ledger-file) --strict -w -d \"d>=[last 45 days]\" reg assets:checking:tk")
          ("reg-07" "%(binary) -f %(ledger-file) --strict -w -d \"d>=[last 45 days]\" reg assets:checking:vt"))))

  (defun gn/quick-calc ()
    (interactive)
    (setq current-prefix-arg '(4)) ; C-u
    (call-interactively 'quick-calc))

  (bind-key "<f7>" #'gn/quick-calc ledger-mode-map)
  (bind-key "C-<f7>" #'quick-calc ledger-mode-map)
  (bind-key "<f8>" #'my-ledger-report/body ledger-mode-map)
  (bind-key "<f9>" #'ledger-mode-clean-buffer ledger-mode-map)
  (bind-key "n" #'next-line ledger-report-mode-map)
  (bind-key "p" #'previous-line ledger-report-mode-map)
  (add-hook 'ledger-report-mode-hook (lambda () (hl-line-mode 1)))

  (defun my-ledger-mode-hook ()
    (setq company-idle-delay 0.2)
    (setq company-tooltip-limit 10)
    (setq company-minimum-prefix-length 2)
    (flycheck-mode 1)
    (company-mode 1)
    (setq pcomplete-ignore-case t)
    (setq completion-ignore-case t))
  (add-hook 'ledger-mode-hook 'my-ledger-mode-hook)

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
_3_: This month     _c_: Cash        _m_: AV
_4_: Last month     _a_: A           _t_: T
_5_: This week      _g_: G           _h_: VT
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
    ("v" (gn/ledger-report "reg-04"))
    ("m" (gn/ledger-report "reg-05"))
    ("t" (gn/ledger-report "reg-06"))
    ("h" (gn/ledger-report "reg-07"))))

(provide 'dot-ledger)
