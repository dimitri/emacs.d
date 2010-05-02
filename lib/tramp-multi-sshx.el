;;; From #emacs

(eval-after-load "tramp"
  (if nil
      ;; The bare-bones implementation.
      '(defun tramp-compute-multi-hops (vec)
         (if (string= "sudo" (or (tramp-file-name-method vec) ""))
             (list (vector "ssh" nil (tramp-file-name-host vec) "") vec)
           (list vec)))

    ;; The implementation that is tacked onto the end of the existing function.
    '(defun tramp-compute-multi-hops (vec)
       "Expands VEC according to `tramp-default-proxies-alist'.
Gateway hops are already opened."
       (let ((target-alist `(,vec))
             (choices tramp-default-proxies-alist)
             item proxy)

         ;; Look for proxy hosts to be passed.
         (while choices
           (setq item (pop choices)
                 proxy (nth 2 item))
           (when (and
                  ;; host
                  (string-match (or (nth 0 item) "")
                                (or (tramp-file-name-host (car target-alist)) ""))
                  ;; user
                  (string-match (or (nth 1 item) "")
                                (or (tramp-file-name-user (car target-alist)) "")))
             (if (null proxy)
                 ;; No more hops needed.
                 (setq choices nil)
               ;; Replace placeholders.
               (setq proxy
                     (format-spec
                      proxy
                      `((?u . ,(or (tramp-file-name-user (car target-alist)) ""))
                        (?h . ,(or (tramp-file-name-host (car target-alist)) "")))))
               (with-parsed-tramp-file-name proxy l
                 ;; Add the hop.
                 (add-to-list 'target-alist l)
                 ;; Start next search.
                 (setq choices tramp-default-proxies-alist)))))

         ;; Handle gateways.
         (when (and (boundp 'tramp-gw-tunnel-method)
                    (string-match (format
                                   "^\\(%s\\|%s\\)$"
                                   (symbol-value 'tramp-gw-tunnel-method)
                                   (symbol-value 'tramp-gw-socks-method))
                                  (tramp-file-name-method (car target-alist))))
           (let ((gw (pop target-alist))
                 (hop (pop target-alist)))
             ;; Is the method prepared for gateways?
             (unless (tramp-get-method-parameter
                      (tramp-file-name-method hop) 'tramp-default-port)
               (tramp-error
                vec 'file-error
                "Method `%s' is not supported for gateway access."
                (tramp-file-name-method hop)))
             ;; Add default port if needed.
             (unless
                 (string-match
                  tramp-host-with-port-regexp (tramp-file-name-host hop))
               (aset hop 2
                     (concat
                      (tramp-file-name-host hop) tramp-prefix-port-format
                      (number-to-string
                       (tramp-get-method-parameter
                        (tramp-file-name-method hop) 'tramp-default-port)))))
             ;; Open the gateway connection.
             (add-to-list
	      'target-alist
	      (vector
	       (tramp-file-name-method hop) (tramp-file-name-user hop)
	       (funcall (symbol-function 'tramp-gw-open-connection) vec gw hop) nil))
             ;; For the password prompt, we need the correct values.
             ;; Therefore, we must remember the gateway vector.  But we
             ;; cannot do it as connection property, because it shouldn't
             ;; be persistent.  And we have no started process yet either.
             (tramp-set-file-property (car target-alist) "" "gateway" hop)))

         ;; Foreign and out-of-band methods are not supported for multi-hops.
         (when (cdr target-alist)
           (setq choices target-alist)
           (while choices
             (setq item (pop choices))
             (when
                 (or
                  (not
                   (tramp-get-method-parameter
                    (tramp-file-name-method item) 'tramp-login-program))
                  (tramp-get-method-parameter
                   (tramp-file-name-method item) 'tramp-copy-program))
               (tramp-error
                vec 'file-error
                "Method `%s' is not supported for multi-hops."
                (tramp-file-name-method item)))))

         ;; In case the host name is not used for the remote shell
         ;; command, the user could be misguided by applying a random
         ;; hostname.
         (let* ((v (car target-alist))
                (method (tramp-file-name-method v))
                (host (tramp-file-name-host v)))
           (unless
               (or
                ;; There are multi-hops.
                (cdr target-alist)
                ;; The host name is used for the remote shell command.
                (member
                 '("%h") (tramp-get-method-parameter method 'tramp-login-args))
                ;; The host is local.  We cannot use `tramp-local-host-p'
                ;; here, because it opens a connection as well.
                (string-match tramp-local-host-regexp host))
             ;; THIS IS THE PART TWB ADDED
             ;; /sudo:x@y:z ==> /multi:sshx:x@y:sudo:root@y:z
             (setq target-alist
                   (cons (vector "sshx"
                                 (unless (string= "root" (tramp-file-name-user v))
                                   (tramp-file-name-user v))
                                 (tramp-file-name-host v)
                                 "")
                         (cons (vector (tramp-file-name-method v)
                                       "root"
                                       (tramp-file-name-host v)
                                       (tramp-file-name-localname v))
                               (cdr target-alist)))))) ; END OF THE PART TWB ADDED
         ;; Result.
         target-alist))))

(provide 'tramp-multi-sshx)

