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

(defcustom dim-doc-mode:make "make"
  "how do you spell GNU Make on your system")

(require 'dim-lib) ; defines with-current-directory
(require 'doc-mode)

(defun dim-doc-mode:display-man-page (target)
  "display given target man page, which is local, not installed on the system"
  (cond ((eq dim-doc-mode:man-reader 'man)
	 ;; man users certainly will prefer to kill pre-existing buffers
	 (let ((man-buffer-name (concat "*Man " target "*")))
	   (when (get-buffer man-buffer-name) (kill-buffer man-buffer-name)))
	 (Man-getpage-in-background target))
	
	((eq dim-doc-mode:man-reader 'woman)
	 (message "woman-find-file %s" target)
	 (woman-find-file target))))

(defun dim-doc-mode:sentinel (process event)
  "a simple process sentinel so that we don't display the man page early"
  (when (eq (process-status process) 'exit)
    (let* ((target (process-get process :target)))
      (dim-doc-mode:display-man-page target))))

(defun dim-doc-mode:compile ()
  "Compile current asciidoc document to a man page"
  (interactive)
  (unless (string-match "\.[15]\.txt" (buffer-file-name))
    (error "%s does not look like an asciidoc manpage source." (buffer-file-name)))

  (save-buffer)

  (let* ((default-directory (file-name-directory (buffer-file-name)))
	 (man (file-name-nondirectory 
	       (file-name-sans-extension (buffer-file-name))))
	 (target (concat (file-name-directory default-directory) man))

	 ;; fed up of M-x compile breakings of window configuration
	 (process
	  (start-process (format "asciidoc to man %s" target)
			 target
			 "make" "-k" "-f" dim-doc-mode:makefile man)))

    (process-put process :target target)
    (set-process-sentinel process 'dim-doc-mode:sentinel)))

(define-key doc-mode-map (kbd "C-c m") 'dim-doc-mode:compile)

(provide 'dim-doc-mode)
