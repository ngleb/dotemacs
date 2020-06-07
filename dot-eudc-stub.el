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

;; (eudc-protocol-set 'eudc-inline-expansion-format '("%s <%s>" displayName email)
;;                    'ldap)
;; 
;; (setq eudc-server-hotlist '(("localhost" . bbdb)
;;                             ("ldap://ldap.example.com" . ldap)))
;; 
;; (setq eudc-default-return-attributes nil
;;       eudc-strict-return-matches nil)
;; (setq ldap-ldapsearch-args '("-LL" "-tt")) ;; default
;; (defun enz-eudc-expand-inline()
;;   (interactive)
;;   (move-end-of-line 1)
;;   (insert "*")
;;   (unless (condition-case nil
;;               (eudc-expand-inline)
;;             (error nil))
;;     (backward-delete-char-untabify 1))))
