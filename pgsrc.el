;;; pgsrc.el --- Source Code Editing Facilities for PostgreSQL
;;
;; Copyright (C) 2011 PostgreSQL Global Development Group
;;
;; Author: Dimitri Fontaine <dimitri@2ndQuadrant.fr>
;; Created: 2011-11-09
;; Keywords: emacs postgresql source code automated editing
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install
;;
;; add the directory to your `load-path' then (require 'pgsrc)
;;
;; Usage
;;
;;   M-x pgsrc:get-node-name RET 900 RET
;;   M-x pgsrc:add-node-support RET 900 RET
;;
;;; Commentary:
;;
;; Given the generic nature of the Node *parsetree data structure in the
;; PostgreSQL sources, the support functions for handling the output and
;; reading back of it are easy enough to deduce from the C structures.
;;
;; This file provides tools to automatically generate the C code needed to
;; add support to a given node of the tree. This node is given as a number,
;; because that's what PostgreSQL will report in its error messages.
;;
;; Here's an example of those:
;;
;;   ERROR:  unrecognized node type: 900
;;
;; Those function will get the node name from parsing the NodeTag enum in
;; src/include/nodes/nodes.h then use that to find their way in outfuncs.c
;; and readfuncs.c.
;;
;; It's necessary to call those functions from a file in the repository
;; where you want to apply their effect --- often enough PostgreSQL hackers
;; will have more than one copy of the repository and its branches sitting
;; around, so a single variable to change would be less usable than just
;; switching to an opened file in there then calling the command.
;;
;; You can call that command from any file of the repository, it will use a
;; git command line to find the top-level directory and work its way from
;; there.
;;
;;; Code:

(require 'cl)				; loop is there

(defun pgsrc:get-tree-root (&optional filename)
  "return the top level directory where current sources are held"
  (let* ((git (executable-find "git"))
	 (cwd (file-name-directory (or filename (buffer-file-name))))
	 (cwd (expand-file-name (file-truename cwd)))
	 (root
	  (when (file-directory-p cwd)
	    (let* ((default-directory (file-name-as-directory cwd))
		   (cdup (car (process-lines git "rev-parse" "--show-cdup"))))
	      (message cdup)
	      (if cdup
		  (if (string-match "^fatal" cdup)
		      (error "%s" cdup)
		    (file-name-as-directory (expand-file-name cdup cwd)))
		cwd)))))
    (unless (file-exists-p (expand-file-name "src/include/nodes/nodes.h" root))
      (error "Please call this command from a PostgreSQL git repository."))
    root))

(defmacro pgsrc:with-source-file (current-filename relative-pathname &rest body)
  "Execute BODY with current buffer containing the content of the
given relative-pathname"
  `(with-temp-buffer
     (insert-file-contents-literally
      (expand-file-name ,relative-pathname
			(pgsrc:get-tree-root ,current-filename)))
     ,@body))

(defun pgsrc:round-figures-list (number)
  "Given 732, returns (700 650 650 600 550 550 500 450 450 400 ... 50)"
  (let* ((zeroes     (expt 10 (truncate (log10 number))))
	 (half       (/ zeroes 2))
	 (multiplier (/ number zeroes)))
    (loop for n from multiplier downto 1
	  for m = (* n zeroes)
	  when (< (+ half m) number) collect (+ half m)
	  collect m
	  collect (- m half))))

(defun pgsrc:current-node-name ()
  "return the node definition of current line, if any, or nil"
  (when (looking-at (rx (* space) "T_" (+ (any alnum "_")) (or space "," eol)))
    (while (looking-at (rx space)) (forward-char))
    (buffer-substring-no-properties
     (point)
     (1- (re-search-forward (rx (or space "," eol)))))))

(defvar previous-node-number 0)

(defun pgsrc:current-node-number ()
  "return current node number, point must be placed with
pgsrc:current-node-name"
  (let ((current-node-number
	 (cond ((looking-at (rx "= " (group (+ digit)) ","))
		(string-to-number (match-string 1)))
	       ((looking-back (rx "T_" (+ (any alnum "_")) ",")
			      (line-beginning-position))
		(1+ previous-node-number))
	       (t nil))))
    (when current-node-number
      (setq previous-node-number current-node-number))))

(defun pgsrc:parse-node-list (filename)
  "returns a list of two vectors indexed the same. The first one
is a list of node names, the second one is the list of node
numbers.

must be called from within a nodes.h buffer"
  (pgsrc:with-source-file filename "src/include/nodes/nodes.h"
    (goto-char (point-min))
    (search-forward "typedef enum NodeTag")
    (forward-char)			; down to next line {
    (forward-line)			; down to the first entry
    (loop while (not (looking-at "}"))
	  if (pgsrc:current-node-name)
	  collect it into names
	  and collect (pgsrc:current-node-number) into numbers
	  do (progn (forward-line) (forward-line 0))
	  finally return (list names numbers))))

;;;###autoload
(defun pgsrc:get-node-name (number &optional filename)
  "Return the T_NodeStmt string from the given node number. When
called interactively (M-x), place the node name in the kill ring.

FILENAME is a filename path from where to look for the current
PostgreSQL source tree you're editing, it defaults to
`buffer-file-name'."
  (interactive "nNode number: ")
  (let* ((filename  (or filename (buffer-file-name)))
	 (stopgaps  (pgsrc:round-figures-list number))
	 (nodelist  (pgsrc:parse-node-list filename))
	 (nodenames (car nodelist))
	 (nodenums  (cadr nodelist))
	 (pos       (position number nodenums))
	 (nodename  (when pos (nth pos nodenames))))
    (message "Node %d is %s." number nodename)
    (when (and nodename (called-interactively-p 'any))
      (kill-new nodename))
    ;; return the node name in all cases
    nodename))

(defun pgsrc:get-node-number (name &optional filename nodelist)
  "Return the number of the given node NAME. NAME can be either
on the form of T_NodeStmt (nodes.h, outfuncs.c) or of
NODESTMT (readfuncs.c)"
  (interactive "MNode name: ")
  (let* ((filename  (or filename (buffer-file-name)))
	 (nodelist  (or nodelist (pgsrc:parse-node-list filename)))
	 (nodenames (car nodelist))
	 (nodenums  (cadr nodelist))
	 (noderegex (concat "^"
			    (if (equal (substring name 0 2) "T_") name
			      (concat "T_" name))
			    "$"))
	 (pos       (position noderegex nodenames :test #'string-match-p))
	 (number    (when pos (nth pos nodenums))))
    (when number
      (message "Node %s is %d in enum NodeTag." name number))
    number))

(defun pgsrc:get-name (string)
  "string might contain a leading *"
  (if (equal (substring string 0 1) "*")
      (substring string 1)
    string))

(defun pgsrc:get-function-body (filename nodename read-or-write)
  "return the function body for inclusion in outfuncs.c or
readfuncs.c for given node, depending on READ-OR-WRITE.

The function body is returned as a list of strings to be
typically written one per line."
  (pgsrc:with-source-file filename "src/include/nodes/parsenodes.h"
    (search-forward (format "typedef struct %s" nodename))
    (forward-char)			; skip to the {
    (forward-line)			; let's go parse the struct
    (forward-line)			; and avoid the NodeTag line
    (mapcar
     (lambda (x)
       (cond ((eq read-or-write 'write)  (concat "WRITE_" x))
	     ((eq read-or-write 'read)   (concat "READ_" x))
	     (t (error "unknown read-or-write value"))))
     (loop for (type name)
	   in (loop while (not (looking-at "}"))
		    when (looking-at (rx (* (any space alnum "*_")) ";"))
		    do (while (looking-at (rx space)) (forward-char))
		    and collect (split-string
				 (buffer-substring-no-properties
				  (point) (1- (search-forward ";"))))
		    do (progn (forward-line) (forward-line 0)))
	   collect (cond ((and (equal type "int") (equal name "location"))
			  "LOCATION_FIELD(location)")
			 ((member type '("bool" "int" "float" "long" "Oid"))
			  (format "%s_FIELD(%s)" (upcase type) name))
			 ((equal type "int32")
			  (format "INT_FIELD(%s)" name))
			 ((and (equal type "char")
			       (equal (substring name 0 1) "*"))
			  (format "STRING_FIELD(%s)" (pgsrc:get-name name)))
			 ((equal type "char")
			  (format "CHAR_FIELD(%s)" (pgsrc:get-name name)))
			 ((save-excursion
			    (search-backward (format "enum %s" type) nil t))
			  (format "ENUM_FIELD(%s,%s)" (pgsrc:get-name name) type))
			 (t
			  (format "NODE_FIELD(%s)" (pgsrc:get-name name))))))))

;;;###autoload
(defun pgsrc:add-node-outfunc-support (number &optional filename nodelist)
  "add the necessary function support to outfuncs.c for node number"
  (interactive "nNode number: ")
  (let* ((filename  (or filename (buffer-file-name)))
	 (outfuncs  (expand-file-name "src/backend/nodes/outfuncs.c"
				     (pgsrc:get-tree-root filename)))
	 (nodelist  (or nodelist (pgsrc:parse-node-list filename)))
	 (nodenames (car nodelist))
	 (nodenums  (cadr nodelist))
	 (pos       (position number nodenums))
	 (nodename  (when pos (nth pos nodenames))))
    (with-current-buffer (find-file-noselect outfuncs)
      (goto-char (point-min))
      ;; skip the first line which is the function prototype
      (search-forward "_outNode(StringInfo str, void *obj)" nil nil 2)
      (if (search-forward (format "case %s:" nodename) nil t)
	  (message "_outNode already has a case label for %s" nodename)
	;; find a good place for the new case label
	(let* ((next-existing-node
		(loop for node in (cdr (member nodename nodenames))
		      when (search-forward (format "case %s:" node) nil t)
		      return node))
	       (beg  (progn (forward-line 0) (point)))
	       (end  (progn (search-forward "break;") (forward-char) (point)))
	       (src  (buffer-substring-no-properties beg end))
	       (next (substring next-existing-node 2))
	       (node (substring nodename 2))
	       (new  (replace-regexp-in-string next node src)))
	  (message "Inserting new node %s just before %s."
		   nodename next-existing-node)
	  (forward-line -3)		; back to the case label
	  (insert new)
	  ;; move to where to write the _outNodeStmt function
	  (goto-char (point-min))
	  (search-forward (format"_out%s" next))
	  (forward-line -2)		; that's where we insert the function
	  (insert (format "
static void
_out%s(StringInfo str, %s *node)
{
	WRITE_NODE_TYPE(\"%s\");

}
" node node (upcase node)))
	  (forward-line -1)		; function body comes here
	  (loop for entry in (pgsrc:get-function-body filename node 'write)
		do (insert "	" entry ";\n"))
	  (save-buffer))))))

;;;###autoload
(defun pgsrc:add-node-readfunc-support (number &optional filename nodelist)
  "add the necessary function support to readfuncs.c for node number"
  (interactive "nNode number: ")
  (let* ((filename  (or filename (buffer-file-name)))
	 (readfunc  (expand-file-name "src/backend/nodes/readfuncs.c"
				      (pgsrc:get-tree-root filename)))
	 (nodelist  (or nodelist (pgsrc:parse-node-list filename)))
	 (nodenames (car nodelist))
	 (nodenums  (cadr nodelist))
	 (pos       (position number nodenums))
	 (nodename  (when pos (nth pos nodenames))))
    (with-current-buffer (find-file-noselect readfunc)
      (goto-char (point-min))
      ;; skip the first line which is the function prototype
      (search-forward "parseNodeString(void)")
      (if (search-forward (format "MATCH(\"%s\"," (upcase nodename)) nil t)
	  (message "parseNodeString already has a MATCH branch for %s" nodename)
	;; find a good place for the new case label
	(let* ((next-existing-node
		(loop for node in (cdr (member nodename nodenames))
		      for unode = (upcase (substring node 2)) ; T_
		      when (search-forward (format "MATCH(\"%s\"," unode) nil t)
		      return node))
	       (beg  (progn (forward-line 0) (point)))
	       (end  (progn (search-forward ";") (forward-char) (point)))
	       (src  (buffer-substring-no-properties beg end))
	       (next (substring next-existing-node 2))
	       (node (substring nodename 2))
	       (new  (replace-regexp-in-string next node src))
	       (new  (replace-regexp-in-string ; if (MATCH("NODENAME", 8))
		      (rx (+ digit) "))") (format "%d))" (length node)) new)))
	  (message "Inserting new node %s just before %s." nodename next)
	  (forward-line -2)		; back to the if MATCH branch
	  (insert new)
	  ;; move to where to write the _readNodeStmt function
	  (goto-char (point-min))
	  (search-forward (format"_read%s" next))
	  (forward-line -2)		; that's where we insert the function
	  (insert (format "
/*
 * _read%s
 */
static %s *
_read%s(void)
{
	READ_LOCALS(%s);


	READ_DONE();
}
" node node node node))
	  (forward-line -3)		; function body comes here
	  (loop for entry in (pgsrc:get-function-body filename node 'read)
		do (insert "	" entry ";\n"))
	  (save-buffer))))))

;;;###autoload
(defun pgsrc:add-node-readfunc-support-by-name (name &optional filename nodelist)
  "add the necessary function support to readfuncs.c for node name"
  (interactive "MNode name: ")
  (let* ((filename  (or filename (buffer-file-name)))
	 (nodelist  (or nodelist (pgsrc:parse-node-list filename)))
	 (number    (pgsrc:get-node-number name)))
    (pgsrc:add-node-readfunc-support number filename nodelist)))

;;;###autoload
(defun pgsrc:add-node-support (number &optional filename)
  "add the necessary function support to outfuncs and readfuncs.c
for given node NUMBER.

FILENAME has to be a file sitting in the git repository you wish
to edit this way, and defaults to the current buffer's file
name (see `buffer-file-name')."
  (interactive "nNode number: ")
  (let* ((filename  (or filename (buffer-file-name)))
	 (nodelist  (pgsrc:parse-node-list filename))
	 (pos       (position number (cadr nodelist)))
	 (nodename  (when pos (nth pos (car nodelist)))))
    (pgsrc:add-node-outfunc-support number filename nodelist)
    (pgsrc:add-node-readfunc-support number filename nodelist)
    (message "Out and Read support added for node \"%s\""  nodename)))

(provide 'pgsrc)

;;; pgsrc.el ends here
