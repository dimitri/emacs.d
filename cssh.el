;;; cssh.el --- clusterssh implementation for emacs

;; Copyright (C) 2007 Phil Hagelberg

;; Author: Phil Hagelberg <technomancy@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/pcmpl-ssh.el
;; Version: 0.2
;; Created: 2007-12-02
;; Keywords: shell completion ssh
;; EmacsWiki: PcompleteSSH

;; This file is NOT part of GNU Emacs.

(require 'pcmpl-ssh)

(defcustom split-horizontally-first t
  "Do we first split horizontally or vertically")

(defun cssh-split-window (&optional backward? &optional size)
  "split current window either vertically or horizontally
depending on split-preference value"
  (let* ((go-horizontal
	 (if backward? (not split-horizontally-first)
	   split-horizontally-first)))

    (if size
	(if go-horizontal
	    (split-window-horizontally size)
	  (split-window-vertically size))

      (if go-horizontal
	  (split-window-horizontally)
	(split-window-vertically)))))

(defun cssh-get-third-size (backward? left top right bottom)
  "Given a window edges and a direction" 
  (let* ((go-horizontal
	 (if backward? (not split-horizontally-first)
	   split-horizontally-first)))

    (/ (+ 1 (if go-horizontal (- right left) (- bottom top))) 3)))

(defun nsplit-window (n &optional backward?)
  "split current window into n windows"
  (let* ((w (selected-window)))

    (cond ((= n 2)
	   (list w (cssh-split-window backward?)))

	  ((= n 3)
	   (let* ((edges (window-edges))
		  (size  (apply #'cssh-get-third-size (cons backward? edges)))
		  (w1    (cssh-split-window backward? size)))

	     (select-window w1)
	     (list w w1 (cssh-split-window backward? size))))

	  ((= 0 (% n 2)) 
	   ;; cut in half then split other parts by n/2
	   (let* ((halves (nsplit-window 2 backward?)))

	     (select-window (nth 1 halves))

	     (let* ((h1l (nsplit-window (/ n 2) (not backward?))))

	       (select-window w)
	       (append halves h1l (nsplit-window (/ n 2) (not backward?))))))

	  ((= 0 (% n 3))
	   ;; cut in three parts then re split
	   (let* ((thirds (nsplit-window 3 backward?)))
	     
	     (select-window (nth 1 thirds))

	     (let* ((t1l (nsplit-window (/ n 3) (not backward?))))

	       (select-window (nth 2 thirds))

	       (let* ((t2l (nsplit-window (/ n 3) (not backward?))))

		 (select-window w)
		 (append thirds 
			 t1l
			 t2l
			 (nsplit-window (/ n 3) (not backward?)))))))

	  (t (message "error: number of windows not a multiple of 2 or 3."))
    )
  )
)

(defun cssh-open (n)
  "open the cssh global input frame then the ssh buffer windows"
  (set-window-buffer (selected-window) (get-buffer-create "*cssh*"))
  (insert "cssh> ")

  (let* ((cssh-controler (split-window-vertically -4)))

    (nsplit-window n)
    (select-window cssh-controler)
  )
)

(provide 'cssh)
