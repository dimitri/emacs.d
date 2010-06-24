;;; dim-lib.el --- Dimitri Fontaine
;;
;; Elisp does not have it all, so here additional lib stuff

;; I don't like the clever while hack to have until, let's hide it
(defmacro until (cond &rest body) `(while (progn ,@body ,cond)))

;; my try at walk-path
(defun walk-path (path fun &optional
		       match-regexp depth-first filter filter-call depth)
  "walk given path recursively, calling fun for each entry

If filter is not nil it's expected to be a function taking
filename, attributes and depth as parameters, returning a
boolean. The (sub)path is walked into only when true.

When filter-call is true, we decide whether to call fun depending
on filter.
"
  (dolist (e (directory-files-and-attributes path t match-regexp))
      (let* ((filename   (car e))
	     (attributes (cdr e))
	     (is-subdir  (file-directory-p filename))
	     (cur-depth  (or depth 0))
	     (walk       (and is-subdir
			      ;; skip . and .. to protect the recursion
			      (not (string-match "/\\.\\.?$" filename))
			      (if (functionp filter) 
				  (funcall filter filename attributes depth)
				t))))
	(when (and walk depth-first)
	  (walk-path filename fun match-regexp depth-first filter
		     filter-call (1+ cur-depth)))

	(when (or (not is-subdir) (not filter-call) (and filter-call walk))
	  (funcall fun filename attributes))

	(when (and walk (not depth-first))
	  (walk-path filename fun match-regexp depth-first filter
		     filter-call (1+ cur-depth))))))

(defun walk-path-list (path &optional match-regexp depth-first ignore-dirs)
  "walk given path and build a list of filenames. Don't walk into ignore-dirs."
  (let ((l))
    (walk-path path
	       (lambda (f a) (add-to-list 'l f))
	       match-regexp
	       depth-first
	       (lambda (f a d) (not (member (file-name-nondirectory f) ignore-dirs)))
	       t)
    l))
      
(provide 'dim-lib)