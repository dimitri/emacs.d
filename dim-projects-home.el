;;; dim-projects.el --- Dimitri Fontaine
;;; On définit les projets locaux
(require 'projects)

;; private (not published, selfish ones)
(project-add "emacs" "/Users/dim/.emacs.d")
(project-add "emacs-dev" "/Users/dim/dev/elisp")
(project-add "confs" "/Users/dim/dev/confs")
(project-add "gpl" "/Users/dim/dev/gpl")
(project-add "hdi" "/Users/dim/dev/hdi")
(project-add "muse" "/Users/dim/dev/muse")

;; public OpenSource stuff
(project-add "pgloader" "/Users/dim/dev/pgloader")
(project-add "skytools-php" "/Users/dim/dev/skytools/pgq-php")
(project-add "skytools" "/Users/dim/devskytools/skytools")
(project-add "prefix" "/Users/dim/dev/prefix")
(project-add "ip4r" "/Users/dim/dev/ip4r/ip4r")
(project-add "pgsql" "/Users/dim/dev/PostgreSQL")
(project-add "pg8.1" "/Users/dim/pgsql/8.1/pgsql")
(project-add "pg8.3" "/Users/dim/pgsql/8.3/pgsql")
(project-add "pg-head" "/Users/dim/pgsql/head/")
(project-add "plproxy" "/Users/dim/dev/plproxy")
(project-add "fr_ops" "/Users/dim/dev/btree_fr_ops")
(project-add "min_update" "/Users/dim/dev/min_update")
(project-add "preprepare" "/Users/dim/dev/preprepare")

(project-add "tmp" "/tmp")
(project-add "temp" "/Users/dim/temp")

(project-add "sinn" "/Users/dim/dev/niftyname/git/sinn/")
(project-add "zinn" "/Users/dim/dev/niftyname/git/zinn/")
(project-add "zinn-clients" "/Users/dim/dev/niftyname/git/zinn-clients")

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

	 ("gpl"            (name . "gpl:"))
	 ("hdi"            (name . "hdi:"))
	 ("muse"           (name . "muse:"))
	 
	 ("Temp"           (or (name . "temp:")
			       (name . "tmp:")))

	 ("sinn"           (name . "sinn:"))
	 ("zinn"           (name . "zinn:"))
	 ("zinn-clients"   (name . "zinn-clients:"))

	 ("TERM"           (mode . term-mode))
	 ("IRC"            (mode . rcirc-mode)))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "Groups")))

(provide 'dim-projects-home)
