;;; dim-doc-mode.el --- Dimitri Fontaine
;;
;; Add C-c m binding to compile asciidoc documents
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/switch-window.el
;; Version: 0.3
;; Created: 2010-04-30
;; Keywords: window navigation
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install:
;;  (require 'dim-doc-mode)

(defcustom dim-doc-mode:makefile "~/dev/emacs.d/asciidoc.mak"
  "Where to find the asciidoc.mak Makefile")

(defcustom dim-doc-mode:man-reader 'woman 
  "man or woman")

(require 'doc-mode)

(defun dim-doc-mode:compile ()
  "Compile current asciidoc document to a man page"
  (interactive)
  (when (string-match "\.[15]\.txt" (buffer-file-name))
    (let* ((d-dir default-directory)
	  (dir (file-name-directory (buffer-file-name)))
	  (man (file-name-nondirectory 
		(file-name-sans-extension (buffer-file-name))))
	  (target (concat (file-name-directory dir) man)))
      (cd dir)
      (compile (concat "make -k -f asciidoc.mak " man))
      (cd d-dir)
      (cond ((eq dim-doc-mode:man-reader 'man)
	     (Man-getpage-in-background target))
	    
	    ((eq dim-doc-mode:man-reader 'woman)
	     (woman-find-file target))))))

    ;; (let ((manpage (file-name-sans-extension
    ;; 		    (file-name-sans-extension (buffer-file-name)))))
    ;;   (shell-command-to-string 
    ;;    (format "make -f %s %s" dim-doc-mode:makefile (buffer-file-name))))))

(define-key doc-mode-map (kbd "C-c m") 'dim-doc-mode:compile)

(provide 'dim-doc-mode)