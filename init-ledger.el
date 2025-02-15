;;; init-ledger.el --- Load ledger-mode configuration -*- lexical-binding: t -*-
;;; Commentary:

;; ledger-mode config

;;; Code:

(use-package ledger-mode
  :mode "\\.ledger\\'"
  :bind (:map ledger-mode-map
         ("<f7>"   . gn/quick-calc)
         ("C-<f7>" . quick-calc)
         ("<f8>"   . my-ledger-report/body)
         ;;("TAB"    . company-indent-or-complete-common)
         :map ledger-report-mode-map
         ("n"      . next-line)
         ("p"      . previous-line)
         ("j"      . next-line)
         ("k"      . previous-line))
  :init
  (defun my-ledger-mode-hook ()
    (flycheck-mode 1)
    ;;(company-mode 1)
    (setq-local company-backends '(company-capf))
    (setq-local tab-always-indent 'complete)
    (setq-local completion-ignore-case t)
    (setq-local ledger-complete-in-steps t)
    (setq-local indent-tabs-mode nil))
  (add-hook 'ledger-mode-hook 'my-ledger-mode-hook)
  :config
  (add-hook 'ledger-report-mode-hook 'hl-line-mode)

  (setq ledger-report-use-header-line t)
  (setq ledger-report-use-strict nil)
  (setq ledger-add-transaction-prompt-for-text nil)

  (setq ledger-reports
        (quote
         (("bal" "%(binary) -f %(ledger-file) --wide balance")
          ("reg" "%(binary) -f %(ledger-file) --wide register")
          ("payee" "%(binary) -f %(ledger-file) --wide register @%(payee)")
          ("account" "%(binary) -f %(ledger-file) --wide register %(account)")
          ("account-last-14days" "%(binary) -f %(ledger-file) --wide -d \"d>=[last 14 days]\" register %(account)")
          ("account-last-30days" "%(binary) -f %(ledger-file) --wide -d \"d>=[last 30 days]\" register %(account)")
          ("account-last-120days" "%(binary) -f %(ledger-file) --wide -d \"d>=[last 120 days]\" register %(account)")
          ("assets-short" "%(binary) -f %(ledger-file) --wide balance \"^assets:cash.*$|^assets:checking.*$|^assets:savings.*$\"")
          ("assets-full" "%(binary) -f %(ledger-file) --wide balance ^assets ^liabilities")
          ("expenses-monthly" "%(binary) -f %(ledger-file) --period %(month) --aux-date --wide balance \"^expenses|^liabilities\"")
          ("expenses-monthly-2" "%(binary) -f %(ledger-file) --period %(month) --aux-date --wide --sort \"-abs(amount)\" --flat balance \"^expenses|^liabilities\"")
          ("budget-monthly" "%(binary) -f %(ledger-file) --period %(month) --aux-date --wide --budget --invert bal ^expenses")
          ("unbudgeted-monthly" "%(binary) -f %(ledger-file) --period %(month) --aux-date --wide --unbudgeted balance ^expenses"))))

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
Balance:               Reports                  Register                       Budget
_a_: Assets (short)    _e_: Monthly expenses    _r_: Account register (14)     _b_: Monthly budget
_f_: Assets (full)     _2_: Sorted expenses     _t_: Account register (30)     _u_: Monthly unbudgeted
_1_: Balance                                    _y_: Account register (120)

_q_ quit"
    ("q" nil)
    ("a" (gn/ledger-report "assets-short"))
    ("f" (gn/ledger-report "assets-full"))

    ("e" (gn/ledger-report "expenses-monthly"))
    ("2" (gn/ledger-report "expenses-monthly-2"))
    ("r" (gn/ledger-report "account-last-14days"))
    ("t" (gn/ledger-report "account-last-30days"))
    ("y" (gn/ledger-report "account-last-120days"))

    ("b" (gn/ledger-report "budget-monthly"))
    ("u" (gn/ledger-report "unbudgeted-monthly"))

    ("1" (gn/ledger-report "bal"))))

(use-package flycheck-ledger
  :after flycheck)



(provide 'init-ledger)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-ledger.el ends here
