(setq user-mail-address "name@example.com"
      user-full-name "First Last")

(setq gnus-select-method '(nnnil ""))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "name"
                      (nnimap-address "imap.example.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      (nnimap-inbox "INBOX")
                      (nnimap-split-methods 'nnimap-split-fancy)
                      (nnimap-split-fancy
                       (| (from "my@example.com" "my")
                          "INBOX.Unsorted"))
                      (nnmail-expiry-target "nnimap+name:Trash")
                      (nnmail-expiry-wait immediate)))

;; Setup to send email through SMTP
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.example.com"
      smtpmail-stream-type 'ssl
      smtpmail-smtp-service 465)

(eval-after-load 'gnus-topic
  '(progn
     (setq gnus-topic-topology '(("Gnus" visible)
                                 (("name" visible nil nil))))

     (setq gnus-topic-alist '(("name" ; the key of topic
                               "nnimap+name:INBOX"
                               "nnimap+name:Drafts"
                               "nnimap+name:Sent Items"
                               "nnimap+name:Junk Mail"
                               "nnimap+name:Trash")
                              ("Gnus")))))

(setq gnus-posting-styles
      '((".*" ;; default style
         (organization "Acme LLC")
         (signature-file "~/.signature") ; text file
         )))

(setq gnus-message-archive-group "nnimap+name:Sent Items")
