(defun exit-gnus-on-exit ()
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))
(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

(with-eval-after-load "mm-decode"
       (add-to-list 'mm-discouraged-alternatives "text/html")
       (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(setq ;; Add date to reply & quote
 message-citation-line-function 'message-insert-formatted-citation-line
 message-citation-line-format "On %a, %b %d %Y, %f wrote:\n")
(setq message-cite-reply-position 'above)

(setq nnimap-split-crosspost nil)
(setq gnus-save-newsrc-file nil)
(setq gnus-permanently-visible-groups ".*")
(setq gnus-use-correct-string-widths nil)
(setq gnus­use­adaptive­scoring nil)

(defun my-message-mode-setup ()
  (setq fill-column 72)
  (turn-on-auto-fill))
(add-hook 'message-mode-hook 'my-message-mode-setup)

(add-hook 'gnus-group-mode-hook 'hl-line-mode)
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Summary/threaded view
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

(cond ((string= (system-name) "lenovo")
       (load-file "~/.emacs.d/lisp/dot-gnus-personal.el"))
      ((string= (system-name) "nga2")
       (load-file "~/.emacs.d/lisp/dot-gnus-work.el")))


(provide 'dot-gnus)

;; Local Variables:
;;   mode: emacs-lisp
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; got-gnus.el ends here
