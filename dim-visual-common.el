;;; dim-visual-common.el --- Dimitri Fontaine

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(scroll-bar-mode nil)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(setq-default indicate-buffer-boundaries 'left)

;; avoid changing existing window configuration for popups
(setq even-window-heights nil)

(line-number-mode 1)
(column-number-mode 1)
(setq-default fill-column 76)
(setq auto-fill-mode 1)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; see what the current selection/region is, allow replacing it
(transient-mark-mode 1)
(delete-selection-mode)

;; show matching parens ([{}]) while moving in the buffers
(show-paren-mode)

;; period single space ends sentence
(setq sentence-end-double-space nil)

;; we want global-hl-line-mode except in M-x term buffers.
(global-hl-line-mode 1)

(defadvice global-hl-line-highlight
  (around dim:global-hl-line-highlight activate)
  (unless (and (eq major-mode 'term-mode) (term-in-char-mode)) ad-do-it))

;; display only tails of lines longer than 80 columns, tabs and
;; trailing whitespaces
(require 'whitespace)
(setq whitespace-line-column 80
      whitespace-style '(face trailing lines-tail))

;; face for tabs long lines' tails
(set-face-attribute 'whitespace-tab nil
		    :background "red1"
		    :foreground "yellow"
		    :weight 'bold)

(set-face-attribute 'whitespace-line nil
		    :background "red1"
		    :foreground "yellow"
		    :weight 'bold)

;; activate minor whitespace mode when in some coding modes
(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'c-mode-hook 'whitespace-mode)

;; quite a stretch, but has its place here too
(setq indent-tabs-mode nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)

;; better completion in C-x 8 RET (unicode input)
(when (boundp 'completion-category-overrides)
  (add-to-list 'completion-category-overrides
               '(unicode-name (styles basic substring))))


;; timestamp *Messages*
;; if you come tired of it
;; (ad-disable-advice 'message 'before 'when-was-that)
;; (ad-update 'message)
(defadvice message (before when-was-that activate)
  "Add timestamps to `message' output."
  (unless (null (ad-get-arg 0))
    (ad-set-arg 0 (concat (format-time-string "%k:%M ") (ad-get-arg 0)))))

(require 'font-lock)
(global-font-lock-mode 1)

(defun get-screen-dimensions (&optional insert)
  "get dimensions of current screen, support X window system and macosx"
  (interactive "p")
  (let ((dims
	 (cond
	  ((string-match "apple-darwin" system-configuration)
	   ;; osascript -e 'tell application \"Finder\" to get bounds of window of desktop'
	   ;; "0, 0, 2560, 1440\n"
	   (mapcar
	    'string-to-number
	    (split-string
	     (substring
	      (shell-command-to-string
	       "osascript -e 'tell application \"Finder\" to get bounds of window of desktop'") 6 -1) ", ")))

	  ((eq window-system 'x)
	   ;; dim ~ xwininfo -root -metric | awk -F '[ x+]' '/geometry/ {print $4, $5}'
	   ;; 1680 1050
	   (mapcar
	    'string-to-number
	    (split-string
	     (substring
	      (shell-command-to-string
	       "xwininfo -root -metric | awk -F '[ x+]' '/geometry/ {print $4, $5}'")
	      0 -1) " ")))

	  ;; other systems are yet unknown
	  (t '(0 0)))))

    ;; Either insert the dimensions, message them, or return them
    (if (called-interactively-p)
	(if (eq insert 1)
	    (message "%sx%s" (car dims) (cadr dims))
	  (insert (format "%sx%s" (car dims) (cadr dims))))
      dims)))

(provide 'dim-visual-common)
