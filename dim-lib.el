;;; dim-lib.el --- Dimitri Fontaine
;;
;; Elisp does not have it all, so here additional lib stuff

;; I don't like the clever while hack to have until, let's hide it
(defmacro until (cond &rest body) `(while (progn ,@body ,cond)))

;; my try at walk-path
(defun walk-path (path fun &optional match-regexp depth-first)
  "walk given path recursively, calling fun for each entry"
  (dolist (e (directory-files-and-attributes path t match-regexp))
      (let* ((filename   (car e))
	     (attributes (cdr e))
	     (is-subdir  (nth 0 attributes)))
	;; skip . and ..
	(unless (string-match "/\\.\\.?$" filename)
	  (if (and is-subdir depth-first)
	      (progn
		(walk-path filename fun match-regexp depth-first)
		(funcall fun filename attributes))

	    (funcall fun filename attributes)
	    (when is-subdir
	      (walk-path filename fun match-regexp depth-first)))))))

(defun walk-path-list (path &optional match-regexp depth-first)
  "walk given path and build a list of (filename . attributes)"
  (let ((l))
    (walk-path path #'(lambda (f a) (add-to-list 'l (cons f a))) 
	       match-regexp depth-first) l))
      
(provide 'dim-lib)