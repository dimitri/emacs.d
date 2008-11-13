;;; cssh.el --- clusterssh implementation for emacs
;;
;; Copyright (C) 2008 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://pgsql.tapoueh.org/elisp
;; Version: 0.1
;; Created: 2008-09-26
;; Keywords: ClusterSSH ssh cssh
;;
;; This file is NOT part of GNU Emacs.
;;
;; Integration:
;;
;; (require 'cssh)
;; (defun turn-on-cssh-binding ()
;;   (local-set-key (kbd "C-=") 'cssh-interactive-start))
;;
;; (add-hook 'ibuffer-mode-hook 'turn-on-cssh-binding)

(require 'pcmpl-ssh)
(require 'ibuffer)

(defcustom split-horizontally-first t
  "Do we first split horizontally or vertically")

;;;
;;; ibuffer interaction: open cssh mode for marked buffers
;;;
(defun cssh-interactive-start ()
  (interactive)
  (cssh-init-from-ibuffer-marked-buffers)
)

(defun cssh-init-from-ibuffer-marked-buffers (&optional cssh-buffer-name)
  "open cssh global input frame and the buffers windows from
marked ibuffers buffers" 
  (cssh-open 
   (if cssh-buffer-name (cssh-buffer-name)
     "*cssh*")
   (ibuffer-get-marked-buffers))
)

;;;
;;; Entry point
;;;
(defun cssh-open (cssh-buffer-name marked-buffers)
  "open the cssh global input frame then the ssh buffer windows"
  (set-window-buffer (selected-window) (get-buffer-create cssh-buffer-name))

  (let* ((cssh-controler (split-window-vertically -4))
	 (cssh-windows (cssh-nsplit-window marked-buffers)))

    (select-window cssh-controler)
    (insert "\ncssh> ")
    '(cssh-windows)
  )
)

;;;
;;; Window splitting code
;;;
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

(defun cssh-nsplit-window (buffer-list &optional backward?)
  "split current window into n windows"
  (let* ((w (selected-window))
	 (n (length buffer-list)))

    (cond ((= n 2)
	   (let* ((w1 (cssh-split-window backward?)))
	     (set-window-buffer w (car buffer-list))
	     (set-window-buffer w1 (cadr buffer-list))
	     (list w w1)))

	  ((= n 3)
	   (let* ((edges (window-edges))
		  (size  (apply #'cssh-get-third-size (cons backward? edges)))
		  (w1    (cssh-split-window backward? size))
		  (w2    (cssh-split-window backward? size)))

	     (set-window-buffer w (car buffer-list))
	     (set-window-buffer w1 (cadr buffer-list))
	     (set-window-buffer w2 (cadr (cdr buffer-list)))
	     (select-window w1)
	     (list w w1 w2)))

	  ((= 0 (% n 2)) 
	   ;; cut in half then split other parts by n/2
	   ;; gives cssh-nsplit-window any 2 elements list
	   (let* ((halves (cssh-nsplit-window ('1 2) backward?)))

	     (select-window (nth 1 halves))

	     (let* ((h1l 
		     (cssh-nsplit-window
		      (butlast buffer-list (/ n 2)) (not backward?))))

	       (select-window w)
	       (append halves 
		       h1l
		       (cssh-nsplit-window 
			(last buffer-list (/ n 2)) (not backward?))))))

	  ((= 0 (% n 3))
	   ;; cut in three parts then re split
	   (let* ((thirds (cssh-nsplit-window 3 backward?)))
	     
	     (select-window (nth 1 thirds))

	     (let* ((t1l (cssh-nsplit-window (/ n 3) (not backward?))))

	       (select-window (nth 2 thirds))

	       (let* ((t2l (cssh-nsplit-window (/ n 3) (not backward?))))

		 (select-window w)
		 (append thirds 
			 t1l
			 t2l
			 (cssh-nsplit-window (/ n 3) (not backward?)))))))

	  ;; n is not divisible by either 2 or 3, produce some more windows
	  ;; than necessary
	  ((= 0 (% (+ 1 n) 2))
	   (cssh-nsplit-window (+ 1 n)))

	  ((= 0 (% (+ 1 n)) 3)
	   (cssh-nsplit-window (+ 1 n)))

	  (t (message "error: number of windows not a multiple of 2 or 3."))
    )
  )
)

(provide 'cssh)
