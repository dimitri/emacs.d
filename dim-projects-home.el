;;; dim-projects.el --- Dimitri Fontaine
;;; On d�finit les projets locaux

(dim:project-merge
 '(
   ;; private (not published, selfish ones)
   ("gpl"		. "~/dev/gpl")
   ("hdi"		. "~/dev/hdi")
   ("Quadrant"	. "~/Quadrant")
   ("yesql"     . "~/dev/yesql/")

   ;; lbc stuff
   ("lbc" . "~/LeBonCoin/gitlab/leboncoin-fr/")

   ;; public OpenSource stuff
   ("pgloader"	. "~/dev/pgloader")
   ("skytools-php" . "~/dev/skytools/pgq-php")
   ("skytools"	. "~/dev/skytools")
   ("preprepare"	. "~/dev/preprepare")
   ("prefix"	. "~/dev/prefix")
   ("base36"	. "~/dev/base36")
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
   ("tcc"	. "~/dev/tcc")
   ("qemu"	. "~/dev/qemu")
   ("vacietis"	. "~/dev/Vacietis")
   ("xlogdump"	. "~/dev/xlogdump")
   ("pginstall" . "~/dev/pginstall")
   ("pgcharts"  . "~/dev/pgcharts")
   ("pgdashboard" . "~/dev/pgdashboard")
   ("plconvert" . "~/dev/plconvert")
   ("ql-to-deb" . "~/dev/ql-to-deb")
   ("kids"      . "~/dev/kids")
   ("cl-ixf"    . "~/dev/CL/cl-ixf")
   ("artisan"   . "~/dev/CL/artisan-mysql/")
   ("decimal"   . "/Users/dim/dev/decimal/")
   ("mastering" . "~/dev/masteringpostgresql.com/")
   ("regresql"  . "~/dev/go/src/github.com/dimitri/regresql/main.go")

   ;; PostgreSQL branches and local test installations
   ("pgsql"	. "~/dev/PostgreSQL")
   ("citus"     . "~/dev/CitusData/citus")
   ("cqueue"    . "~/dev/CitusData/cqueue/")
   ("tpch"      . "~/dev/CitusData/tpch/")
   ("citus-ha"  . "~/dev/CitusData/citus-ha/")
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
   ("pgpatch"	. "~/dev/PostgreSQL/patches")

   ;; Books
   ("yesql"      . "~/dev/yesql")

   ;; 2ndQuadrant stuff
   ("2ndQDev"    . "~/dev/2ndQ")
   ("2ndQForm"   . "~/dev/2ndQ/courses")
   ("2ndQDocs"   . "~/dev/2ndQ/2ndq-asciidoc")
   ("2ndQTools"  . "~/dev/2ndQ/tools")
   ("2ndQMgt"    . "~/2ndQuadrant")
   ("2ndiMac"    . "~/Documents/Quadrant-iMac")
   ("2ndMBP"     . "~/Documents/Quadrant-MBP")
   ("2ndQLBC"    . "~/2ndQuadrant/customer_code/lbc/")
   ("2ndQMJ"     . "~/2ndQuadrant/customer_code/mailjet/")

   ;; 2ndQuadrant per customer stuff
   ("parch"      . "~/2ndQuadrant/Local/Clients/Le-Bon-Coin/archive_DBA/parch")

   ;; Some Common Lisp stuff
   ("sudoku"	. "~/dev/CL/sudoku")
   ("dsl"	. "~/dev/CL/dsl-in-lisp")
   ("fl-com"	. "~/dev/CL/fotolog-comments")
   ("jiaroo"	. "~/dev/CL/jiaroo")
   ("qmynd"	. "~/dev/Common-Lisp/qmynd/")
   ;; ("pgcloader"	. "~/dev/CL/pgloader")
   ("QL"        . "~/quicklisp/local-projects")

   ;; lost-oasis open source
   ("sinn"		. "~/dev/niftyname/git/sinn/")
   ("zinn"		. "~/dev/niftyname/git/zinn/")
   ("zinn-clients" . "~/dev/niftyname/git/zinn-clients")))

(provide 'dim-projects-home)
