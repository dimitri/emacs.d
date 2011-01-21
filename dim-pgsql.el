;;; dim-pgsql.el --- Dimitri Fontaine
;;
;; PostgreSQL source code
;;
;; we used to have pgsql-linum-format here, see dim-packages.el instead

(add-hook 'c-mode-hook
	  (function
	   (lambda nil
	     (when (and buffer-file-name
			(or (string-match "pgsql" buffer-file-name)
			    (string-match "pgext" buffer-file-name)
			    (string-match "postgresql" buffer-file-name)))
	       (c-set-style "bsd")
	       (setq c-basic-offset 4)
	       (setq tab-width 4)
	       (c-set-offset 'case-label '+)
	       ;; (setq show-trailing-whitespace t) ; we now use whitespace-mode
	       (setq indent-tabs-mode t)))))

;;; To work on the documentation, the following (or a variant, as above)
;;; can be helpful.

(defun pgsql-sgml-mode ()
  "SGML mode adjusted for PostgreSQL project"
  (interactive)
  (sgml-mode)
  (setq indent-tabs-mode nil)
  (setq sgml-basic-offset 1))

(setq auto-mode-alist
  (cons '("\\(postgres\\|pgsql\\).*\\.sgml\\'" . pgsql-sgml-mode)
        auto-mode-alist))

(provide 'dim-pgsql)
