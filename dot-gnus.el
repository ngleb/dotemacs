(setq gnus-select-method '(nnnil ""))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gmane.io"))

(setq gnus-save-newsrc-file nil)
(setq gnus-permanently-visible-groups ".*")

(add-hook 'gnus-group-mode-hook 'hl-line-mode)
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-20,20f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
;; gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
 gnus-thread-sort-functions '(gnus-thread-sort-by-date)
 gnus-sum-thread-tree-indent "  "
 gnus-sum-thread-tree-root "● "
 gnus-sum-thread-tree-false-root "◯ "
 gnus-sum-thread-tree-single-indent "◎ "
 gnus-sum-thread-tree-vertical "│"
 gnus-sum-thread-tree-leaf-with-other "├─► "
 gnus-sum-thread-tree-single-leaf "╰─► "
 gnus-summary-display-arrow t)
