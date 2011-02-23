;;; dim-desktop.el --- Dimitri Fontaine
;;
;; Allows to prepare a readahead file list from desktop-save

(require 'desktop)

(defvar dim-desktop-file-readahead-list
  ".emacs.desktop.readahead"
  "*Where to save the emacs desktop `readahead` file list")

(defvar dim-desktop-filelist-command
  "gawk -F '[ \"]' '/desktop-.*-buffer/ {getline; if($4) print $4}' %s"
  "Command to run to prepare the readahead file list")

(defun dim-desktop-get-readahead-file-list (&optional filename dir)
  "get the file list for readahead from dekstop file in DIR, or ~"
  (with-temp-file (or filename dim-desktop-file-readahead-list)
    (insert
     (shell-command-to-string
      (format dim-desktop-filelist-command
	      (expand-file-name desktop-base-file-name (or dir "~")))))))

;; This will not work because the hook is run before to add the buffers into
;; the desktop file.
;;
;;(add-hook 'desktop-save-hook 'dim-desktop-get-readahead-file-list)

;; so instead, advise the function
(defadvice desktop-save (after desktop-save-readahead activate)
  "Prepare a readahead(8) file for the desktop file"
  (dim-desktop-get-readahead-file-list))

(provide 'dim-desktop)
