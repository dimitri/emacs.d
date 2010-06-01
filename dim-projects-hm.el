;;; On définit les projets locaux

(dim:project-merge
 '(("admin"		. "~/Hi-Media/admin/admin")
   ("UT"		. "~/Hi-Media/admin/admin/UT")
   ("dba-site"		. "~/Hi-Media/dba/")
   ("hm-min-update"	. "~/Hi-Media/admin/admin/PostgreSQL/min-update")
   ("pg_staging"	. "~/PostgreSQL/pgfoundry/pg_staging")

   ;; Hi-Media
   ("comtrack"		. "~/Hi-Media/comtrack")
   ("HMEurovox"		. "~/Hi-Media/Eurovox")
   ("Eurovox"		. "~/Hi-Media/admin/git/Eurovox")
   ("Allopass"		. "~/Hi-Media/AlloPass")
   ("ApCode"	        . "~/Hi-Media/AlloPass/svn/alloscripts_lib/stable/sql/apcode/")
   ("MediaReporting"	. "~/Hi-Media/MediaReporting")
   ("DataManager"	. "~/Hi-Media/Partenaires")
   ("HPMP"		. "~/Hi-Media/hpmp")
   ("AdLink"            . "~/Hi-Media/AdLink")

   ;; PostgreSQL / pgfoundry
   ("pgsql"             . "~/PostgreSQL/postgresql")
   ("pgext"             . "~/PostgreSQL/postgresql-extension")
   ("pgconfs"           . "~/dev/pgconfs")
   ("pgloader"		. "~/PostgreSQL/pgfoundry/pgloader") 
   ("libphp-pgq"	. "~/PostgreSQL/pgfoundry/libphp-pgq")
   ("skytools"		. "~/PostgreSQL/pgfoundry/skytools")
   ("skytools-3"	. "~/PostgreSQL/skytools-3/skytools-dev")
   ("prefix"		. "~/PostgreSQL/pgfoundry/prefix")
   ("pgbouncer"		. "~/PostgreSQL/pgfoundry/pgbouncer")
   ("tablelog"		. "~/PostgreSQL/pgfoundry/table_log-0.4.4")
   ("pgfouine"		. "~/PostgreSQL/pgfoundry/pgfouine-1.0")
   ("pgsql"		. "~/PostgreSQL/git/postgresql")
   ("pg8.2"		. "~/pgsql/8.2")
   ("pg8.3"		. "~/pgsql/8.3")
   ("pg8.4"		. "~/pgsql/8.4")
   ("pghead"		. "~/pgsql/head")
   ("plproxy"		. "~/PostgreSQL/pgfoundry/plproxy")
   ("fr_ops"		. "~/Hi-Media/admin/admin/PostgreSQL/btree_fr_ops")
   ("period"		. "~/PostgreSQL/pgfoundry/temporal")
   ("pre_prepare"	. "~/PostgreSQL/pgfoundry/preprepare")
   ("temporal"		. "~/PostgreSQL/pgfoundry/temporal")
   ("pitrtools"		. "~/PostgreSQL/pgfoundry/pitrtools")
   ("hstore"		. "~/PostgreSQL/pgfoundry/hstore")
   ("min_update"	. "~/PostgreSQL/pgfoundry/backport/min_update")
   ("uuid"		. "~/PostgreSQL/pgfoundry/backport/uuid")
   ("uuid-ossp"		. "~/PostgreSQL/pgfoundry/backport/uuid-ossp")
   ("pgdeb"		. "~/PostgreSQL/debian")
   ("pgexamples"        . "~/PostgreSQL/examples")

   ;; emacs related projects
   ("cssh"		. "~/dev/emacs/cssh")
   ("rcirc-groups"	. "~/dev/emacs/rcirc-groups")

   ;; misc projects
   ("tsung-plotter"	. "~/dev/tsung-plotter")
   ("hdi"		. "~/dev/hdi")
   ("dalidoc"		. "~/dev/dalidoc")
   ("sql2dot"		. "~/dev/sql2dot")
   ("mbot"		. "~/dev/mbot-0.3")

   ;; others projects
   ("django"		. "/usr/share/pyshared/django/")
   ("transifex"		. "~/Hi-Media/transifex")

   ;; local pg clusters /etc files
   ("pg-etc"		. "~/pgsql")

   ;; setup
   ("~xdg"		. "~/.config")
   ("xdg"		. "/etc/xdg")
   ("ssh"		. "~/.ssh")))

(dim:add-my-extra-load-paths '("~/Hi-Media/dba"))
(require 'dba-site)

(provide 'dim-projects-hm)
