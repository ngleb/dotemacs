(defun exit-gnus-on-exit ()
  (if (and (fboundp 'gnus-group-exit)
           (gnus-alive-p))
      (with-current-buffer (get-buffer "*Group*")
        (let (gnus-interactive-exit)
          (gnus-group-exit)))))
(add-hook 'kill-emacs-hook 'exit-gnus-on-exit)

(setq message-directory "~/my/gnus/Mail")
(setq nnfolder-directory "~/my/gnus/Mail/archive")

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
;; gnus-thread-sort-functions '(gnus-thread-sort-by-date)
 gnus-sum-thread-tree-indent "  "
 gnus-sum-thread-tree-root "● "
 gnus-sum-thread-tree-false-root "◯ "
 gnus-sum-thread-tree-single-indent "◎ "
 gnus-sum-thread-tree-vertical "│"
 gnus-sum-thread-tree-leaf-with-other "├─► "
 gnus-sum-thread-tree-single-leaf "╰─► "
 gnus-summary-display-arrow t)

(setq user-mail-address "name@example.com"
      user-full-name "First Last")

(setq gnus-select-method '(nnnil ""))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "example"
                      (nnimap-address "imap.example.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      (nnimap-inbox "INBOX")
                      (nnimap-split-methods 'nnimap-split-fancy)
                      (nnimap-split-fancy
                       (| (from "my@example.com" "my")
                          "INBOX"))
                      (nnmail-expiry-target "nnimap+example:Trash")
                      (nnmail-expiry-wait immediate)))

;; Setup to send email through SMTP
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.example.com"
      ;;smtpmail-stream-type 'ssl
      smtpmail-smtp-service 25) ;; 456 for ssl

;; (eval-after-load 'gnus-topic
;;   '(progn
;;      (setq gnus-topic-topology '(("Gnus" visible)
;;                                  (("example" visible nil nil))))

;;      (setq gnus-topic-alist '(("example" ; the key of topic
;;                                "nnimap+example:INBOX"
;;                                "nnimap+example:Drafts"
;;                                "nnimap+example:Sent Items"
;;                                "nnimap+example:Junk Mail"
;;                                "nnimap+example:Trash")
;;                               ("Gnus")))))

(setq gnus-posting-styles
      '((".*" ;; default style
         (organization "ACME LLC")
         (signature "Yours sincerely,\nFirst Last")
         )))

(setq gnus-message-archive-group "nnimap+example:Sent Items")


;;; EUDC
(use-package eudc
  :config
  (use-package message
    :config
    (define-key message-mode-map (kbd "TAB") 'eudc-expand-inline))
  (setq eudc-strict-return-matches nil)

  (eudc-protocol-set 'eudc-inline-query-format '((cn)
                                                 (mail)
                                                 (cn cn)
                                                 (cn cn cn)
                                                 (sn)
                                                 (givenName)
                                                 (givenName name)
                                                 (name))
                     'ldap)

  (setq eudc-server-hotlist '(("ldap://ldap.example.com" . ldap)))
  (setq ldap-host-parameters-alist '(("ldap://ldap.example.com"
                                      base "ou=_Users,dc=DOMAIN"
                                      binddn "domain\\name"
                                      auth simple
                                      scope subtree
                                      passwd ldap-password-read)))
  (setq eudc-inline-expansion-servers 'hotlist)
  (eudc-set-server "ldap://ldap.example.com" 'ldap t))

;; (eudc-protocol-set 'eudc-inline-expansion-format '("%s <%s>" displayName email) 'ldap)
;; (setq eudc-server-hotlist '(("localhost" . bbdb)
;;                             ("ldap://ldap.example.com" . ldap)))
;; (setq eudc-default-return-attributes nil
;;       eudc-strict-return-matches nil)
;; (setq ldap-ldapsearch-args '("-LL" "-tt")) ;; default


(provide 'dot-gnus)

;; Local Variables:
;;   mode: emacs-lisp
;;   outline-regexp: "^;;;_\\([,. ]+\\)"
;; End:

;;; got-gnus.el ends here
