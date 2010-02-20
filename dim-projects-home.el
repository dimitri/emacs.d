;;; dim-projects.el --- Dimitri Fontaine
;;; On définit les projets locaux
(require 'projects)

;; private (not published, selfish ones)
(project-add "emacs" "~/.emacs.d")
(project-add "emacs-dev" "~/dev/elisp")
(project-add "confs" "~/dev/confs")
(project-add "gpl" "~/dev/gpl")
(project-add "hdi" "~/dev/hdi")
(project-add "muse" "~/dev/muse")

;; public OpenSource stuff
(project-add "pgloader" "~/dev/pgloader")
(project-add "skytools-php" "~/dev/skytools/pgq-php")
(project-add "skytools" "~/devskytools/skytools")
(project-add "prefix" "~/dev/prefix")
(project-add "ip4r" "~/dev/ip4r/ip4r")
(project-add "pgsql" "~/dev/PostgreSQL")
(project-add "pg8.1" "~/pgsql/8.1/pgsql")
(project-add "pg8.3" "~/pgsql/8.3/pgsql")
(project-add "pg-head" "~/pgsql/head/")
(project-add "plproxy" "~/dev/plproxy")
(project-add "fr_ops" "~/dev/btree_fr_ops")
(project-add "min_update" "~/dev/min_update")
(project-add "preprepare" "~/dev/preprepare")
(project-add "pgstaging" "~/dev/pg_staging")

(project-add "tmp" "/tmp")
(project-add "temp" "~/temp")

(project-add "sinn" "~/dev/niftyname/git/sinn/")
(project-add "zinn" "~/dev/niftyname/git/zinn/")
(project-add "zinn-clients" "~/dev/niftyname/git/zinn-clients")

(setq ibuffer-saved-filter-groups
      '(("Groups"
	 ("emacs"          (or (name . "emacs:")
			       (name . "emacs-dev:")
			       (filename . ".emacs")))
	 ("confs"          (name . "confs:"))
	 ("pgloader"       (name . "pgloader:"))
	 ("skytools-php"   (name . "skytools-php:"))
	 ("skytools"       (name . "skytools:"))
	 ("prefix"         (name . "prefix:"))
	 ("ip4r"           (name . "ip4r:"))
	 ("pgsql"          (name . "pgsql:"))
	 ("pg8.1"          (name . "pg8.1:"))
	 ("pg8.3"          (name . "pg8.3:"))
	 ("plproxy"        (name . "plproxy:"))
	 ("fr_ops"         (name . "fr_ops:"))
	 ("min_update"     (name . "min_update:"))
	 ("preprepare"     (name . "preprepare:"))
	 ("pgstaging"      (name . "pgstaging:"))

	 ("gpl"            (name . "gpl:"))
	 ("hdi"            (name . "hdi:"))
	 ("muse"           (name . "muse:"))
	 
	 ("Temp"           (or (name . "temp:")
			       (name . "tmp:")))

	 ("sinn"           (name . "sinn:"))
	 ("zinn"           (name . "zinn:"))
	 ("zinn-clients"   (name . "zinn-clients:"))

	 ("GNUS"           (or (mode . message-mode)
			       (mode . mail-mode)
			       (mode . gnus-group-mode)
			       (mode . gnus-summary-mode)
			       (mode . gnus-article-mode)))

	 ("TERM"           (mode . term-mode))
	 ("IRC"            (mode . rcirc-mode))
	 ("dired"          (mode . dired-mode)))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "Groups")))

(provide 'dim-projects-home)
