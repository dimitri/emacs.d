;;; dim-open-line.el --- some additional shortcuts
;;
;; See http://www.emacswiki.org/cgi-bin/wiki?OpenNextLine

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

;; Behave like vi's o command
(defun dim:open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Behave like vi's O command
(defun dim:open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "M-o") 'dim:open-next-line)      ; used to be open-line
(global-set-key (kbd "C-o") 'dim:open-previous-line)  ; used to be a prefix


(provide 'dim-open-line)
