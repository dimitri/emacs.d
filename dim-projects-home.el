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
   ("skytools"	. "~/devskytools/skytools")
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
   
   ;; lost-oasis open source
   ("sinn"		. "~/dev/niftyname/git/sinn/")
   ("zinn"		. "~/dev/niftyname/git/zinn/")
   ("zinn-clients" . "~/dev/niftyname/git/zinn-clients")))

(provide 'dim-projects-home)
