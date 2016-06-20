;;;
;;; http://www.emacswiki.org/emacs/HippieExpand
;;;

;;;
;;; Don't add extra paren when expanding line with paredit
;;;
(defadvice he-substitute-string (after he-paredit-fix)
  "remove extra paren when expanding line in paredit"
  (if (and paredit-mode (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

;;;
;;; Flexible match expansion
;;;

(defun try-expand-flexible-abbrev (old)
  "Try to complete word using flexible matching.

Flexible matching works by taking the search string and then
interspersing it with a regexp for any character. So, if you try
to do a flexible match for `foo' it will match the word
`findOtherOtter' but also `fixTheBoringOrange' and
`ifthisisboringstopreadingnow'.

The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (unless old
    (he-init-string (he-lisp-symbol-beg) (point))
    (unless (he-string-member he-search-string he-tried-table)
      (setq he-tried-table (cons he-search-string he-tried-table)))
    (setq he-expand-list
          (and (not (equal he-search-string ""))
               (he-flexible-abbrev-collect he-search-string))))
  (while (and he-expand-list
	      (he-string-member (car he-expand-list) he-tried-table))
    (pop he-expand-list))
  (prog1
      (not (null he-expand-list))
    (if (null he-expand-list)
        (when old (he-reset-string))
      (let ((found (pop he-expand-list)))
        (he-substitute-string found)))))

(defun he-flexible-abbrev-collect (str)
  "Find and collect all words that flex-matches STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (let ((collection nil)
        (regexp (he-flexible-abbrev-create-regexp str)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp regexp nil t)
        ;; Is there a better or quicker way than using
        ;; `thing-at-point' here?
        (setq collection (cons (thing-at-point 'symbol) collection))))
    collection))

(defun he-flexible-abbrev-create-regexp (str)
  "Generate regexp for flexible matching of STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (concat "\\b" (mapconcat (lambda (x) (concat "\\w*-*" (list x))) str "")
          "\\w*-*" "\\b"))

(add-to-list 'hippie-expand-try-functions-list
             (defun selective-try-expand-flexible-abbrev (old)
               ;; don't do that in rcirc modes
               (unless (eq major-mode 'rcirc-mode)
                 (try-expand-flexible-abbrev old))))

(provide 'dim-hippie-expand)
