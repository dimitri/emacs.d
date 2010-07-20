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
	(message "walk-path: walk into %S: %S" filename walk)
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

;; try using walk-path-list to guess what file to open
;;
;; that's a kind of useless experiment, so it's broken and known to be.
;;
(defun walk-path-predicate (match)
  "return nil unless there's a match"
  t)

(defun walk-path-build-regexp (string)
  "return a regexp for fuzzy matching filenames"
  (substring (mapconcat 'identity (split-string string "") ".*") 2))
  
(defun walk-path-complete (string predicate flags)
  "return a list of candidates"
  (let* ((string-filename  (file-name-nondirectory string))
	 (string-directory (directory-file-name (file-name-directory string)))
	 (file-regexp      (walk-path-build-regexp string-filename))
	 (dir-regexp       (walk-path-build-regexp string-directory))
	 (match-regexp     (rx-to-string `(or (regexp ,file-regexp)
					      (regexp ,dir-regexp))))
	 (matches))

    (message "walk-path-complete: %S %S %S %S" 
	     string file-regexp dir-regexp match-regexp)

    (walk-path default-directory 
	       (lambda (f a) (add-to-list 'matches f)) 
	       match-regexp
	       nil
	       (lambda (f a d) (string-match dir-regexp f)))

    (cond ((null flags)
	   ;; try-completion
	   (cond ((eq 1 (length matches)) t)
		 ((endp matches) nil)
		 (t (try-completion "" matches))))
	  
	  ((eq flags 'lambda)
	   ;; test-completion
	   (member string matches))
	  
	  (t
	   ;; all-completions
	   matches))))

;(all-completions "cs" 'walk-path-complete)
;(try-completion "cs" 'walk-path-complete)
;(completing-read "Find file: " 'walk-path-complete 'walk-path-predicate)

(provide 'dim-lib)
