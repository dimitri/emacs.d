;;; On définit les projets locaux
(require 'projects)

(project-add "admin" "/home/dim/Hi-Media/admin/admin")
(project-add "hm-min-update" 
	     "/home/dim/Hi-Media/admin/admin/PostgreSQL/min-update")
(project-add "pg_staging" 
	     "/home/dim/PostgreSQL/pgfoundry/pg_staging")

(project-add "comtrack" "/home/dim/Hi-Media/comtrack")
(project-add "HMEurovox" "/home/dim/Hi-Media/Eurovox")
(project-add "Eurovox" "/home/dim/Hi-Media/admin/git/Eurovox")
(project-add "Allopass" "/home/dim/Hi-Media/AlloPass")
(project-add "MediaReporting" "/home/dim/Hi-Media/MediaReporting")
(project-add "DataManager" "/home/dim/Hi-Media/Partenaires")
(project-add "HPMP" "/home/dim/Hi-Media/hpmp")

(project-add "emacs" "/home/dim/.elisp")
(project-add "emacs-dev" "/home/dim/dev/elisp")
(project-add "tapoueh" "/home/dim/dev/tapoueh.org")
(project-add "pgloader" "/home/dim/PostgreSQL/pgfoundry/pgloader")
(project-add "libphp-pgq" "~/PostgreSQL/pgfoundry/libpgq-php")
(project-add "skytools" "/home/dim/PostgreSQL/pgfoundry/skytools")
(project-add "skytools-3" "/home/dim/PostgreSQL/skytools-3/skytools-dev")
(project-add "prefix" "/home/dim/PostgreSQL/pgfoundry/prefix")
(project-add "pgbouncer" "/home/dim/PostgreSQL/pgfoundry/pgbouncer")
(project-add "tablelog" "/home/dim/PostgreSQL/pgfoundry/table_log-0.4.4")
(project-add "pgfouine" "/home/dim/PostgreSQL/pgfoundry/pgfouine-1.0")
(project-add "pgsql" "~/PostgreSQL/git/postgresql")
(project-add "pg8.2" "~/pgsql/8.2")
(project-add "pg8.3" "~/pgsql/8.3")
(project-add "pghead" "~/pgsql/head")
(project-add "plproxy" "/home/dim/PostgreSQL/pgfoundry/plproxy")
(project-add "fr_ops" "/home/dim/Hi-Media/admin/admin/PostgreSQL/btree_fr_ops")
(project-add "period" "/home/dim/PostgreSQL/pgfoundry/temporal")
(project-add "pre_prepare" "/home/dim/PostgreSQL/pgfoundry/preprepare")
(project-add "temporal" "/home/dim/PostgreSQL/pgfoundry/temporal")
(project-add "pitrtools" "/home/dim/PostgreSQL/pgfoundry/pitrtools")
(project-add "hstore" "/home/dim/PostgreSQL/pgfoundry/hstore")
(project-add "min_update" "/home/dim/PostgreSQL/pgfoundry/backports/min_update")
(project-add "uuid" "/home/dim/PostgreSQL/pgfoundry/backports/uuid")
(project-add "uuid-ossp" "/home/dim/PostgreSQL/pgfoundry/backports/uuid-ossp")

;; misc projects
(project-add "tsung-plotter" "/home/dim/dev/tsung-plotter")
(project-add "hdi" "/home/dim/dev/hdi")
(project-add "dalidoc" "/home/dim/dev/dalidoc")
(project-add "sql2dot" "/home/dim/dev/sql2dot")
(project-add "mbot" "/home/dim/dev/mbot-0.3")

;; local pg clusters /etc files
(project-add "pg-etc" "/home/dim/pgsql")

;; setup
(project-add "~xdg" "/home/dim/.config")
(project-add "xdg" "/etc/xdg")

(project-add "temp" "/tmp")

(setq ibuffer-saved-filter-groups
      '(("Groups"
	 ("admin"          (name . "admin:"))
	 ("hm-min-update"  (name . "hm-min-update"))
	 ("pg_staging"     (name . "pg_staging"))

	 ("Eurovox"        (or (name . "Eurovox:")
			       (name . "HMEurovox:")))
	 ("comtrack"       (name . "comtrack:"))
	 ("Allopass"       (name . "Allopass:"))
	 ("MediaReporting" (name . "MediaReporting:"))
	 ("DataManager"    (name . "DataManager:"))
	 ("hpmp"           (name . "HPMP:"))
	 
	 ("emacs"          (or (name . "emacs:")
			       (name . "emacs-dev:")
			       (filename . ".emacs")))
	 ("tapoueh"        (name . "tapoueh:"))
	 ("pgloader"       (name . "pgloader:"))
	 ("libphp-pgq"     (name . "libphp-pgq:"))
	 ("skytools"       (name . "skytools:"))
	 ("skytools-3"     (name . "skytools-3:"))
	 ("prefix"         (name . "prefix:"))
	 ("pgbouncer"      (name . "pgbouncer:"))

	 ("min_update"     (name . "min_update:"))
	 ("uuid"           (name . "uuid:"))
	 ("uuid-ossp"      (name . "uuid-ossp:"))

	 ("tablelog"       (name . "tablelog:"))
	 ("pgfouine"       (name . "pgfouine:"))
	 ("pgsql"          (name . "pgsql:"))

	 ("pg8.2"          (name . "pg8.2:"))
	 ("pg8.3"          (name . "pg8.3:"))
	 ("pghead"         (name . "pghead:"))

	 ("plproxy"        (name . "plproxy:"))
	 ("fr_ops"         (name . "fr_ops:"))
	 ("period"         (name . "period:"))
	 ("pre_prepare"    (name . "pre_prepare:"))
	 ("pitrtools"      (name . "pitrtools:"))
	 ("temporal"       (name . "temporal:"))
	 ("hstore"         (name . "hstore:"))
	 ("pg-etc"         (name . "pg-etc"))

	 ("misc"           (or (name . "tsung-plotter:")
			       (name . "hdi:")
			       (name . "dalidoc:")
			       (name . "sql2dot:")
			       (name . "mbot:")))

	 ("xdg"            (or (name . "~xdg:")
			       (name . "xdg")))

	 ("Temp"           (name . "temp:"))
	 
	 ("IRC"            (mode . rcirc-mode))
	 ("Terms"          (mode . term-mode))
	 ("Gnus"           (or (mode . gnus-group-mode)
			       (mode . gnus-summary-mode)
			       (mode . gnus-article-mode)
			       (mode . message-mode))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "Groups")))

(provide 'dim-projects-hm)
