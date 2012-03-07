;;; wget.el --- fetch an URL content into a local file
;;
;; Contrary to what the name suggests this uses url-retrieve rather than
;; calling the system's wget tool.

(require 'url)

(defun wget (&optional url filename)
  "Download URL into FILENAME"
  (interactive
   (list (read-minibuffer "URL: ")
	 (read-minibuffer "Filename: ")))

  (when (file-exists-p filename)
    (if (yes-or-no-p (format "Replace \"%s\" content? " filename))
	(delete-file filename)
      (error "File \"%s\" already exists." filename)))

  (with-temp-file filename
    (insert
     (with-current-buffer (url-retrieve-synchronously url)
       ;; prune HTTP headers
       (goto-char (point-min))
       (re-search-forward "^$" nil 'move)
       (forward-char)
       (delete-region (point-min) (point))
       (buffer-string)))))
