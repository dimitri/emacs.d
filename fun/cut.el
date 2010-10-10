;;; cut.el --- Dimitri Fontaine
;;
;; http://programmingpraxis.com/2010/08/17/cut/
;;
(eval-when-compile (require 'cl))

(defun dim:cut (mode list &optional delimiter)
  "Implement Unix cut in Emacs Lisp. For the fun of it."
  (unless (member mode '(char field))
    (error "Cut operates in `char' or `field' mode only."))
  (let* ((output (get-buffer-create "*cut*"))
	 (ranges (mapcar (lambda (x)
			   ;; split ranges, 1-4,5,6-8
			   (if (string-match "-" x) 
			       (mapcar 'string-to-int (split-string x "-"))
			     (list (string-to-int x) (string-to-int x))))
			 (split-string list ",")))
	 (content (mapcar (lambda (line)
			    (if (eq mode 'char) line
			      (split-string line (or delimiter "\t"))))
			  (split-string
			   (buffer-substring-no-properties (point-min) (point-max)) "\n"))))
    (with-current-buffer output
      (erase-buffer)
      (insert
       (loop for line in content
	     concat (concat 
		     (loop for (b e) in ranges
			   concat (concat 
				   (if (eq mode 'char)
				       (if (> (length line) e)
					   (substring line (- b 1) (- e 1))
					 (when (> (length line) b)
					   (substring line (- b 1))))
				     ;; field based cutting
				     (loop for i from b to e
					   concat (nth (- i 1) line)))
				   (when (eq mode 'field) (or delimiter "\t"))))
		     "\n"))))
    (set-window-buffer (selected-window) output)))

(defun cut (ranges &optional delimiter)
  "Interactive caller for dim:cut"
  (interactive (list (read-string "ranges: ")
		     (unless current-prefix-arg
		       (read-char "delimiter: "))))
  (dim:cut (if current-prefix-arg 'char 'field) 
	   ranges
	   (unless current-prefix-arg (char-to-string delimiter))))
