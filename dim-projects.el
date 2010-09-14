;;; dim-projects.el --- Dimitri Fontaine
;;
;; load the right projects definition file, provide project-merge and
;; ibuffer integration

(require 'ibuffer)
(require 'ibuf-ext)
(require 'projects)
(require 'dim-ports)
(require 'cl)

(defcustom dim:my-projects-common '(("tmp"         . "/tmp")
				    ("temp"        . "~/temp")
				    ("emacs"       . "~/dev/emacs.d")
				    (".emacs.d"    . "~/.emacs.d")
				    ("cssh"        . "~/dev/emacs/cssh")
				    ("el-get"      . "~/dev/emacs/el-get")
				    ("el-get.d"    . "~/.emacs.d/el-get")
				    ("rcirc-groups". "~/dev/emacs/rcirc-groups")
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
				     
(defun dim:add-projects-and-setup-ibuffer-groups (&optional project-alist)
  "Apply dim:my-projects to both project-add and ibuffer-saved-filter-groups."

  ;; add projects
  (mapc (lambda (x) (project-add (car x) (cdr x))) project-alist)

  ;; tweak ibuffer-saved-filter-groups
  ;; hard code the '(("Groups" (list (of . setups))))
  (setq ibuffer-saved-filter-groups
	`(("Groups" 
	   ;; retain existing values
	   ,@(cdar ibuffer-saved-filter-groups)

	   ;; add new projects, avoid duplicates
	   ,@(set-difference 
	      (mapcar
	       (lambda (x) 
		 (dim:build-ibuffer-groups 
		  (car x) (list (concat (car x) ":"))))
	       project-alist)
	      ibuffer-saved-filter-groups
	      :key 'car
	      :test 'equal)))))

(defun dim:project-merge (project-alist)
  "merge the given list of projects with the current installed one"
  ;; remove projects to merge from current project list, so that we use the
  ;; new directory if it changed
  (let ((duplicates 
	 (mapcar 
	  'car 
	  (intersection project-alist project-root-alist :key 'car :test 'equal))))
    (message "dim:project-merge removing %S" duplicates)
    (mapc 'project-remove duplicates))

  ;; now install the new projects
  (dim:add-projects-and-setup-ibuffer-groups
   (set-difference project-alist project-root-alist :key 'car :test 'equal)))

;; Now we load the common definitions
(dim:add-projects-and-setup-ibuffer-groups dim:my-projects-common)

;; the following files will use dim:project-merge to add their setup
(if (get-domain-name)
    (if (string-match "hi-media-techno" (get-domain-name))
	(require 'dim-projects-hm)
      (require 'dim-projects-home))
  (error "get-domain-name is nil, can't load local projects."))

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
