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
   ("preprepare"	. "~/dev/preprepare")
   ("prefix"	. "~/dev/prefix")
   ("ip4r"		. "~/dev/ip4r/ip4r")
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
   ("myfdw"	. "~/dev/mysql_fdw")
   ("pgextwlist"	. "~/dev/pg_ext_whitelist")
   ("pgbouncer"	. "~/dev/pgbouncer")
   ("plproxy"	. "~/dev/plproxy")
   ("pgextwlist"	. "~/dev/pgextwlist")
   ("PartMgr"	. "~/dev/PartMgr")

   ;; PostgreSQL branches and local test installations
   ("pgsql"	. "~/dev/PostgreSQL")
   ("pgd8.1"	. "~/pgsql/8.1/pgsql")
   ("pgd8.3"	. "~/pgsql/8.3/pgsql")
   ("pgd8.4"	. "~/pgsql/8.4/pgsql")
   ("pgd9.0"	. "~/pgsql/9.0/pgsql")
   ("pgd9.1"	. "~/pgsql/9.1/pgsql")
   ("pgd9.2"	. "~/pgsql/9.2/pgsql")
   ("pgdddl"	. "~/pgsql/ddl/pgsql")
   ("pgsql"	. "~/dev/PostgreSQL/postgres")
   ("pg8.3"	. "~/dev/PostgreSQL/pg8.3")
   ("pg8.4"	. "~/dev/PostgreSQL/pg8.4")
   ("pg9.0"	. "~/dev/PostgreSQL/pg9.0")
   ("pg9.2"	. "~/dev/PostgreSQL/pg9.2")

   ;; Some Common Lisp stuff
   ("sudoku"	. "~/dev/CL/sudoku")
   ("dsl"	. "~/dev/CL/dsl-in-lisp")
   ("fl-com"	. "~/dev/CL/fotolog-comments")
   ("jiaroo"	. "~/dev/CL/jiaroo")
   ("pgcloader"	. "~/dev/CL/pgloader")

   ;; lost-oasis open source
   ("sinn"		. "~/dev/niftyname/git/sinn/")
   ("zinn"		. "~/dev/niftyname/git/zinn/")
   ("zinn-clients" . "~/dev/niftyname/git/zinn-clients")))

(provide 'dim-projects-home)
