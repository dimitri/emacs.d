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
;;
;; TODO
;;  * add documentation
;;
;; BUGS
;;  init of interface for 9 buffers is buggy (creates too much windows)
;;

(require 'pcmpl-ssh)
(require 'ibuffer)
(require 'term)

(defcustom split-horizontally-first t
  "Do we first split horizontally or vertically")

(defcustom cssh-prompt "cssh> "
  "cssh buffer prompt")

(defcustom cssh-default-buffer-name "*cssh*"
  "cssh default buffer name, the one in cssh major mode")

;;;
;;; ibuffer interaction: open cssh mode for marked buffers
;;;
(defun cssh-interactive-start (&optional cssh-buffer-name)
  (interactive)
  (cssh-init-from-ibuffer-marked-buffers cssh-buffer-name)
)

(defun cssh-init-from-ibuffer-marked-buffers (&optional cssh-buffer-name)
  "open cssh global input frame and the buffers windows from
marked ibuffers buffers" 
  (let* ((buffers-all-in-term-mode t)
	 (marked-buffers (ibuffer-get-marked-buffers)))
    
    (dolist (elt marked-buffers)
      (progn
	(message (buffer-name elt))
	;;(select-window (get-buffer-window elt))
	(with-current-buffer elt
	  (when (not (eq major-mode 'term-mode))
	    (progn
	      (setq buffers-all-in-term-mode nil)
	      (message "ClusterSSH only supports Term mode buffers"))))))

    (when buffers-all-in-term-mode
      (cssh-open 
       (if cssh-buffer-name (cssh-buffer-name) cssh-default-buffer-name)
       marked-buffers))))

;;;
;;; Entry point
;;;
(defun cssh-open (cssh-buffer-name marked-buffers)
  "open the cssh global input frame then the ssh buffer windows"
  (cond ((eq 1 (length marked-buffers))
	 (set-window-buffer (selected-window) (car marked-buffers)))
	 
	(t
	 (set-window-buffer (selected-window) (get-buffer-create cssh-buffer-name))

	 (let* ((cssh-controler (split-window-vertically -4))
		(cssh-windows (cssh-nsplit-window marked-buffers)))

	   (select-window cssh-controler)
	   (insert (concat "\n" cssh-prompt))
	   (set (make-local-variable 'cssh-buffer-list) marked-buffers)
	   (set (make-local-variable 'cssh-window-list) cssh-windows)
	   (cssh-mode)
	   ;; return the windows list
	   '(cssh-windows)))))

;;;
;;; cssh editing mode
;;;
(defvar cssh-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [up]        'cssh-send-up)
    (define-key map [down]      'cssh-send-down)
    (define-key map [tab]       'cssh-send-tab)
    (define-key map (kbd "RET") 'cssh-send-input)
    (define-key map (kbd "C-j") 'cssh-send-input)
    (define-key map (kbd "C-m") 'cssh-send-input)
    (define-key map (kbd "C-c") 'cssh-cancel-input)
    (define-key map (kbd "C-l") 'cssh-clear)    
    (define-key map (kbd "C-=") 'cssh-reopen)
    map)
  "Keymap for `cssh-mode'.")

;;;###autoload
(define-derived-mode cssh-mode fundamental-mode "ClusterSSH"
  "A major mode for controlling multiple terms at once."
  (set (make-local-variable 'cssh-buffer-list) marked-buffers)
  (set (make-local-variable 'cssh-window-list) cssh-windows))

;;
;; Input functions
;;
(defun cssh-cancel-input ()
  (interactive)
  (insert (concat "\n" cssh-prompt)))

(defun cssh-send-string (string)
  "generic function to send input to the terms"
  (let* ((w (selected-window)))

    (dolist (elt cssh-buffer-list)
      ;; FIXME: get rid of artefacts elements in cssh-buffer-list
      (when (bufferp elt)
	(progn (select-window (get-buffer-window elt))
	       (insert string)
	       (term-send-input))))

    (select-window w)))

(defun cssh-send-defun (term-fun)
  "generic function to apply term function to the terms"
  (let* ((w (selected-window)))

    (dolist (elt cssh-buffer-list)
      ;; FIXME: get rid of artefacts elements in cssh-buffer-list
      (when (bufferp elt)
	(progn (select-window (get-buffer-window elt))
	       (funcall term-fun))))

    (select-window w)))

(defun cssh-send-up ()
  (interactive)
  (cssh-send-defun 'term-send-up))

(defun cssh-send-down ()
  (interactive)
  (cssh-send-defun 'term-send-down))

(defun cssh-send-tab ()
  (interactive)
  (cssh-send-string
   (buffer-substring (+ (length cssh-prompt) (line-beginning-position))
		     (line-end-position)))
  (cssh-send-string "\C-i"))

(defun cssh-send-input ()
  "send current line content to all cssh-mode buffers"
  (interactive)
  (cssh-send-string
   (buffer-substring (+ (length cssh-prompt) (line-beginning-position))
		     (line-end-position)))
  (insert (concat "\n" cssh-prompt)))

(defun cssh-clear ()
  (interactive)
  (cssh-send-string "clear"))

(defun cssh-reopen ()
  (interactive)
  (cssh-open (buffer-name) cssh-buffer-list))

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

	     (when (bufferp (car buffer-list))
	       (set-window-buffer w (car buffer-list)))

	     (when (bufferp (cadr buffer-list))
	       (set-window-buffer w1 (cadr buffer-list)))

	     (list w w1)))

	  ((= n 3)
	   (let* ((edges (window-edges))
		  (size  (apply #'cssh-get-third-size (cons backward? edges)))
		  (w1    (cssh-split-window backward? size))
		  (w2    (progn (select-window w1) 
				(cssh-split-window backward? size))))

	     (when (bufferp (car buffer-list))
	       (set-window-buffer w (car buffer-list)))

	     (when (bufferp (cadr buffer-list))
	       (set-window-buffer w1 (cadr buffer-list)))

	     (when (bufferp (cadr (cdr buffer-list)))
	       (set-window-buffer w2 (cadr (cdr buffer-list))))

	     (list w w1 w2)))

	  ((= 0 (% n 2)) 
	   ;; cut in half then split other parts by n/2
	   ;; gives cssh-nsplit-window any 2 elements list
	   (let* ((halves (cssh-nsplit-window '(1 2) backward?)))

	     (select-window (nth 1 halves))

	     (let* ((h1l 
		     (cssh-nsplit-window
		      (butlast buffer-list (/ n 2)) (not backward?))))

	       (select-window w)
	       (append h1l
		       (cssh-nsplit-window 
			(last buffer-list (/ n 2)) (not backward?))))))

	  ((= 0 (% n 3))
	   ;; cut in three parts then re split
	   (let* ((thirds (cssh-nsplit-window '(1 2 3) backward?)))
	     
	     (select-window (nth 1 thirds))

	     (let* ((t1l (cssh-nsplit-window
			  (butlast buffer-list (/ n 3)) (not backward?))))

	       (select-window (nth 2 thirds))

	       (let* ((t2l (cssh-nsplit-window
			    (last buffer-list (/ n 3)) (not backward?))))

		 (select-window w)
		 (append t1l
			 t2l
			 (cssh-nsplit-window
			  (last (butlast buffer-list (/ n 3)) (/ n 3))
			  (not backward?)))))))

	  ;; n is not divisible by either 2 or 3, produce some more windows
	  ;; than necessary
	  ((= 0 (% (+ 1 n) 2))
	   (cssh-nsplit-window (cons 1 buffer-list)))

	  ((= 0 (% (+ 1 n)) 3)
	   (cssh-nsplit-window (cons 1 buffer-list)))

	  (t (message "error: number of windows not a multiple of 2 or 3."))
    )
  )
)

(provide 'cssh)
