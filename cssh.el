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

(defun cssh-split-window (&optional backward?)
  "split current window either vertically or horizontally
depending on split-preference value"
  (let* ((go-horizontal
	 (if backward? (not split-horizontally-first)
	   split-horizontally-first)))

  (if go-horizontal
      (split-window-horizontally)
    (split-window-vertically))))

(defun nsplit-window (n)
  "split current window into n windows"
  (let* ((w (selected-window)))

    (cond ((= n 2) (cssh-split-window))
	  ((<= n 4)
	   (select-window (cssh-split-window))
	   (cssh-split-window t)
	   (select-window w)
	   (cssh-split-window t)
	  )
	  ((<= n 8)
	   (select-window (nsplit-window 2))
	   (nsplit-window 4)
	   (select-window w)
	   (nsplit-window 4)
	  )
	  (t (message "cssh currently does not support that many windows."))
    )
  )
)

(provide 'cssh)
