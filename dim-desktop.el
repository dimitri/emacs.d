;;; dim-desktop.el --- Dimitri Fontaine
;;
;; Allows to prepare a readahead file list from desktop-save

(require 'desktop)

(defvar dim-desktop-file-readahead-list
  "/tmp/readahead.emacs.early"
  "*Where to save the emacs desktop `readahead` file list")

(defvar dim-desktop-filelist-command
  "gawk 'n==1{if(!/nil/)print gensub(/\"/, \"\", \"g\", $1);n=0} /desktop-.*-buffer/{n=1}' %s"
  "Command to run to prepare the readahead file list")

(defun dim-desktop-get-readahead-file-list (&optional filename dir)
  "get the file list for readahead from dekstop file in DIR, or ~"
  (with-temp-file (or filename dim-desktop-file-readahead-list)
    (insert
     (shell-command-to-string
      (format dim-desktop-filelist-command
	      (expand-file-name desktop-base-file-name (or dir "~")))))))

(add-hook 'desktop-save-hook 'dim-desktop-get-readahead-file-list)

(provide 'dim-desktop)
