(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ledger-reports
   (quote
    (("bal" "ledger -f %(ledger-file) --strict bal")
     ("reg" "ledger -f %(ledger-file) --strict reg")
     ("payee" "ledger -f %(ledger-file) --strict reg @%(payee)")
     ("account" "ledger -f %(ledger-file) --strict reg %(account)")
     ("balass" "ledger -f %(ledger-file) --strict bal assets and not \\(Foreign\\)")
     ("balexpthismonth" "ledger -f %(ledger-file) --strict -p \"this month\" bal expenses")
     ("balexplastmonth" "ledger -f %(ledger-file) --strict -p \"last month\" bal expenses")
     ("balexpthisweek" "ledger -f %(ledger-file) --strict -p \"this week\" --start-of-week=1 bal expenses")
     ("balexplastweek" "ledger -f %(ledger-file) --strict -p \"last week\" --start-of-week=1 bal expenses")
     ("regaccount" "ledger -f %(ledger-file) --strict -p \"from last week\" --start-of-week=1 reg %(account)"))))
 '(package-selected-packages
   (quote
    (smooth-scrolling zenburn-theme yasnippet use-package org-plus-contrib olivetti nlinum markdown-mode magit ledger-mode flyspell-popup flycheck ess dired+ deft counsel))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
