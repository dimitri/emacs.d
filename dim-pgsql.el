;;; dim-pgsql.el --- Dimitri Fontaine
;;
;; PostgreSQL source code
;;
;; we used to have pgsql-linum-format here, see dim-packages.el instead

(add-hook 'c-mode-hook
	  (defun dim:c-pgsql-mode-hook ()
	    (when (and buffer-file-name
		       (or (string-match "pgsql" buffer-file-name)
			   (string-match "pgsrc" buffer-file-name)
			   (string-match "pgext" buffer-file-name)
			   (string-match "fdw" buffer-file-name)
			   (string-match "pgtreeagg" buffer-file-name)
			   (string-match "postgres" buffer-file-name)
			   (string-match "postgresql" buffer-file-name)))
	      (c-set-style "bsd")
	      (setq c-basic-offset 4)
	      (setq tab-width 4)
	      (c-set-offset 'case-label '+)
	      (setq fill-column 79)
	      (setq indent-tabs-mode t))))

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

;;
;; From Peter Eisentraut, see the following message
;; http://archives.postgresql.org/message-id/1342042526.2712.21.camel@vanquo.pezone.net
;;
(defun pgsql-perl-style ()
  "Perl style adjusted for PostgreSQL project"
  (interactive)
  (setq tab-width 4)
  (setq perl-indent-level 4)
  (setq perl-continued-statement-offset 4)
  (setq perl-continued-brace-offset 4)
  (setq perl-brace-offset 0)
  (setq perl-brace-imaginary-offset 0)
  (setq perl-label-offset -2))

(add-hook 'perl-mode-hook
           (lambda ()
	     (when (and buffer-file-name
			(or (string-match "pgsql" buffer-file-name)
			    (string-match "pgsrc" buffer-file-name)
			    (string-match "pgext" buffer-file-name)
			    (string-match "fdw" buffer-file-name)
			    (string-match "postgresql" buffer-file-name)))
	       (pgsql-perl-style))))

;; Set the SQL mode to PostgreSQL always
(add-hook 'sql-mode-hook
	  (defun jd:sql-mode-set-sql-product ()
	    (sql-set-product 'postgres)))

(require 'pgsrc)
(provide 'dim-pgsql)
