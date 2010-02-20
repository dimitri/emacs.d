;;; dim-projects.el --- Dimitri Fontaine
;; load the right projects definition file
(require 'projects)
(require 'dim-ports)

(defcustom dim:my-projects-common '(("tmp"         . "/tmp")
				    ("temp"        . "~/temp")
				    ("emacs"       . "~/dev/emacs.d")
				    (".emacs.d"    . "~/.emacs.d")
				    ("confs"       . "~/dev/confs")
				    ("tapoueh.org" .  "~/dev/tapoueh.org"))
  "List of the common custom projects, will get used with
project-add and ibuffer-saved-filter-groups."
  :type '(alist :key-type 'string :value-type 'directory)
  :require 'projects)

(defcustom dim:my-projects '()
  "List of the local custom projects, will get used with
project-add and ibuffer-saved-filter-groups."
  :type '(alist :key-type 'string :value-type 'directory)
  :require 'projects)

(defun dim:build-ibuffer-groups (group &optional names &optional modes)
  "Return a new ibuffer-saved-filter-groups list with parameters added"
  (let ((name-clauses '())
	(mode-clauses '()))
    (when names (setq name-clauses (mapcar (lambda (name) `(name . ,name)) names)))
    (when modes (setq mode-clauses (mapcar (lambda (mode) `(mode . ,mode)) modes)))

    (list
     group 
     (cond ((and name-clauses mode-clauses) `(or ,@name-clauses ,@mode-clauses))

	   ((and name-clauses (cdr name-clauses)) `(or ,@name-clauses))
	   (name-clauses (car name-clauses))

	   ((and mode-clauses (cdr mode-clauses)) `(or ,@mode-clauses))
	   (mode-clauses (car mode-clauses))
	   (t '())))))
				     
(defun dim:setup-my-projects (&optional project-alist)
  "Apply dim:my-projects to both project-add and ibuffer-saved-filter-groups."

  ;; add projects
  (mapc (lambda (x) (project-add (car x) (cdr x))) project-alist)

  ;; tweak ibuffer-saved-filter-groups
  ;; hard code the '(("Groups" (list (of . setups))))
  (setq ibuffer-saved-filter-groups
	`(("Groups" 
	   ,@(mapcar (lambda (x) 
		       (dim:build-ibuffer-groups (car x)
						 (list (concat (car x) ":"))))
		     project-alist)))))
;;
;; the following files should only add projects to dim:my-projects
;;
(if (string-match "hi-media-techno" (get-domain-name))
    (require 'dim-projects-hm)
  (require 'dim-projects-home))

;;
;; and now we can use the previous funcion definitions to load our projects
;;
(dim:setup-my-projects `(,@dim:my-projects-common ,@dim:my-projects))

;;
;; finally, add some common setups (mode dependant)
;;
(setq ibuffer-saved-filter-groups
      `(("Groups" 
	 ,@(cdar ibuffer-saved-filter-groups)

	 ,(dim:build-ibuffer-groups "gnus" nil '(message-mode
						 mail-mode
						 gnus-group-mode
						 gnus-summary-mode
						 gnus-article-mode))
	 ,(dim:build-ibuffer-groups "TERM"  nil '(term-mode))
	 ,(dim:build-ibuffer-groups "IRC"   nil '(rcirc-mode rcirc-groups-mode))
	 ,(dim:build-ibuffer-groups "dired" nil '(dired-mode)))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "Groups")))

(provide 'dim-projects)
