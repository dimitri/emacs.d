;;; dim-projects.el --- Dimitri Fontaine
;;; On définit les projets locaux

(dim:project-merge
 '(
   ;; private (not published, selfish ones)
   ("gpl"		. "~/dev/gpl")
   ("hdi"		. "~/dev/hdi")
   ("Quadrant"	. "~/Quadrant")

   ;; public OpenSource stuff
   ("pgloader"	. "~/dev/pgloader")
   ("skytools-php" . "~/dev/skytools/pgq-php")
   ("skytools"	. "~/dev/skytools/skytools")
   ("prefix"	. "~/dev/prefix")
   ("ip4r"		. "~/dev/ip4r/ip4r")
   ("pgsql"	. "~/dev/PostgreSQL")
   ("pg8.1"	. "~/pgsql/8.1/pgsql")
   ("pg8.3"	. "~/pgsql/8.3/pgsql")
   ("pg-head"	. "~/pgsql/head/")
   ("plproxy"	. "~/dev/plproxy")
   ("fr_ops"	. "~/dev/btree_fr_ops")
   ("min_update"	. "~/dev/min_update")
   ("preprepare"	. "~/dev/preprepare")
   ("pgstaging"	. "~/dev/pg_staging")
   ("libphp-pgq"	. "~/dev/libphp-pgq")
   ("preprepare"	. "~/dev/preprepare")
   ("pgfincore"	. "~/dev/pgfincore")
   ("apt.pg"	. "~/dev/apt.postgresql.org")
   ("getddl"	. "~/dev/getddl")
   ("pgextwlist"	. "~/dev/pg_ext_whitelist")
   ("pgsql"	. "~/dev/PostgreSQL/postgres")

   ;; lost-oasis open source
   ("sinn"		. "~/dev/niftyname/git/sinn/")
   ("zinn"		. "~/dev/niftyname/git/zinn/")
   ("zinn-clients" . "~/dev/niftyname/git/zinn-clients")))

(provide 'dim-projects-home)
