;;; wget.el --- fetch an URL content into a local file
;;
;; Contrary to what the name suggests this uses url-retrieve rather than
;; calling the system's wget tool.

(require 'url)

(defun wget (&optional url filename)
  "Download URL into FILENAME"
  (interactive "MURL: ")

  (insert
   (with-current-buffer (url-retrieve-synchronously url)
     ;; prune HTTP headers
     (goto-char (point-min))
     (re-search-forward "^$" nil 'move)
     (forward-char)
     (delete-region (point-min) (point))
     (buffer-string))))
