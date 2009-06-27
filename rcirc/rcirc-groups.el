;;; rcirc-groups.el -- an emacs buffer in rcirc-groups major mode
;;
;; Copyright (c) 2009 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://pgsql.tapoueh.org/elisp/rcirc
;; Version: 0.1
;; Created: 2009-06-27
;; Keywords: IRC rcirc notify
;;
;; This file is NOT part of GNU Emacs.
;;
;; Emacs Integration:
;; (require 'rcirc-groups)
;;
;; TODO: draw a bar showing where you stoped reading (hook to when the
;; buffer becomes non visible?)

(require 'rcirc)

(defgroup rcirc-groups nil "rcirc-groups customization group"
  :group 'convenience)

(defcustom rcirc-groups:buffer-name "*rcirc-groups*"
  "buffer name where to collect the notifications"
  :group 'rcirc-groups)

(defcustom rcirc-groups:time-format "%Y-%m-%d %H:%M"
  "format string to use when displaying the time of later notification in *rcirc-groups*"
  :group 'rcirc-groups)

(defvar rcirc-groups:conversation-alist nil
  "An alist of conversation buffers and the number of times they mentionned your nick.")

(defvar rcirc-groups-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")      'rcirc-groups:switch-to-conversation-buffer)
    (define-key map (kbd "g")        'rcirc-groups:refresh-conversation-alist)
    (define-key map (kbd "c")        'rcirc-groups:catchup-conversation)
    (define-key map (kbd "C")        'rcirc-groups:catchup-all-conversations)
    (define-key map (kbd "l")        'rcirc-groups:list-mentionned-conversations)
    (define-key map (kbd "L")        'rcirc-groups:list-all-conversations)
    
    map)
  "Keymap for `rcirc-groups-mode'.")

;;;###autoload
(define-derived-mode rcirc-groups-mode fundamental-mode "rcirc-groups-mode"
  "A major mode for handling rcirc notifications"
  :group 'rcirc-groups

  ;; prepare a new conversion listing, and display it
  (rcirc-groups:refresh-conversation-alist)
  (rcirc-groups:list-mentionned-conversations))

(defun rcirc-groups:switch-to-conversation-buffer ()
  "switch from *rcirc-groups* buffer to referenced one"
  (interactive)
  (let ((conversation-buffer) 
	(buffer-substring (line-beginning-position) (line-end-position)))
    (set-window-buffer (selected-window) conversation-buffer)))

(defun rcirc-groups:refresh-conversation-alist ()
  "fill in rcirc-groups:conversation-alist all active conversations"
  (dolist (elt (buffer-list))
    (with-current-buffer elt
      (when (and (eq major-mode 'rcirc-mode))
	(if (assoc elt rcirc-groups:conversation-alist)
	    (push elt rcirc-groups:conversation-alist)
	  (push (cons elt (cons 0 (time-to-seconds (current-time))))
		rcirc-groups:conversation-alist)))))
  rcirc-groups:conversation-alist)

(defun rcirc-groups:catchup-conversation ()
  "catchup conversation reinits the conversation-alist entry for current buffer"

  (let* ((conversation-buffer (buffer-substring
				(line-beginning-position) (line-end-position)))
	 (conversation-entry (assoc conversation-buffer rcirc-groups:conversation-alist)))
    
    (push (cons conversation-buffer (cons 0 (time-to-seconds (current-time))))
	  rcirc-groups:conversation-alist)))

(defun rcirc-groups:catchup-all-conversations ()
  "catchup all conversation reinits all conversation-alist entries"
  (message "Not yet implemented"))

(defun rcirc-groups:list-mentionned-conversations ()
  "list all conversations where some notification has not yet been acknowledged"
  (erase-buffer)
  (dolist (elt rcirc-groups:conversation-alist)
    (when (> (cadr elt) 0)
      (insert (propertize (car elt) 
			  'line-prefix 
			  (format "%s %s "
				  (format-time-string rcirc-groups:time-format (cddr elt))
				  (cadr elt))
			  'face 'rcirc-nick-in-message))
      (insert "\n"))))

(defun rcirc-groups:list-all-conversations ()
  "list all conversations where some notification has not yet been acknowledged"
  (erase-buffer)
  (dolist (elt rcirc-groups:conversation-alist)
    (insert (propertize (car elt) 
			'line-prefix 
			(format "%s %s "
				(format-time-string rcirc-groups:time-format (cddr elt))
				(cadr elt))))
    (insert "\n")))

(defun rcirc-groups:privmsg (proc sender response target text)
  "update the rcirc-groups:conversation-alist counters"
  (when (and (string= response "PRIVMSG")
             (not (string= sender (rcirc-nick proc)))
             (not (rcirc-channel-p target))
             (rcirc-notify-mode:nick-allowed sender))

    (let ((conversation (assoc (current-buffer) rcirc-groups:conversation-alist)))
      (push (cons (car conversation) (cons (+ 1 (cadr conversation)) 
					   (time-to-seconds (current-time))))
	    rcirc-groups:conversation-alist))))

(defun rcirc-groups:notify-me (proc sender response target text)
  "update the rcirc-groups:conversation-alist counters"
  (interactive)

  (when (and (string-match (concat (rcirc-nick proc) "[:, $]") text)
	     ;(not (string= (rcirc-nick proc) sender))
             (not (string= (rcirc-server-name proc) sender))
             (rcirc-notify-mode:nick-allowed sender))

    (let ((conversation (assoc (current-buffer) rcirc-groups:conversation-alist)))
      (push (cons (car conversation) (cons (+ 1 (cadr conversation)) 
					   (time-to-seconds (current-time))))
	    rcirc-groups:conversation-alist))))

(defun rcirc-groups:create-notify-buffer ()
  "Create the rcirc-groups:buffer-name buffer in read-only"
  (let ((groups-buffer (get-buffer-create rcirc-groups:buffer-name)))
    (with-current-buffer groups-buffer
      (setq buffer-read-only t)
      ;; ensure we're in the rcirc-groups major mode -- we could just
      ;; be creating the buffer.
      (unless (eq major-mode 'rcirc-groups-mode)
	(rcirc-groups-mode)))
    groups-buffer))

(defun rcirc-groups:switch-to-groups-buffer ()
  "switch to the groups buffer"
  (interactive)
  (let ((groups-buffer (rcirc-groups:create-notify-buffer)))
    (set-window-buffer (selected-window) groups-buffer)))

(add-hook 'rcirc-print-hooks 'rcirc-groups:privmsg)
(add-hook 'rcirc-print-hooks 'rcirc-groups:notify-me)
(add-hook 'rcirc-mode-hook   (lambda ()
			       (local-set-key (kbd "C-c g") 
					      'rcirc-groups:switch-to-groups-buffer)))

(provide 'rcirc-groups)
