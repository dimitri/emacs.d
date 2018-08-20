;;; dim-init-macosx.el --- Dimitri Fontaine
;;
(setenv "LANG" "en_US.UTF-8")

(require 'info)
(add-to-list 'Info-directory-list
	     (concat invocation-directory "../Resources/info"))
(add-to-list 'Info-directory-list "/sw/share/info/")

;; special visual tweaking
(setq color-theme-libraries nil)
(load "~/.emacs.d/el-get/color-theme/color-theme.el")
(require 'color-theme)

;(require 'color-theme-emacs23-default)
;(color-theme-emacs23-default)

(require 'color-theme-tango-2)
(color-theme-tango-2)
;; (set-face-attribute 'hl-line nil :background "steelblue4")
;; (set-face-attribute 'hl-line nil :background "skyblue4")
(set-face-attribute 'hl-line nil :background "grey20")

;; here hiding the menu bar makes no sense
(menu-bar-mode 1)

(require 'woman)
(add-to-list 'woman-manpath "/sw/share/man")

;; chuuut
(setq ring-bell-function 'ignore)

;; MacOSX specific setting
(setq mac-allow-anti-aliasing t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)
;(ns-set-background-alpha 0.9)
(add-to-list 'default-frame-alist '(alpha . 94))

;; font, taille et position
;(set-face-font 'default "Monaco-13")
;(set-face-font 'default "DejaVu Sans Mono-14")
(set-face-font 'default "Andale Mono-14")
(set-frame-position (selected-frame) 60 30)

(defvar dim:frame-parameters
  '((left   . ((  10 30) (174 84)))
    (right  . ((1430 30) (130 84)))
    (single . ((  10 30) (168 52))))	; used in 1440x900
  "Usual frame parameters")

(defun dim:set-frame-parameters (frame name)
  "Apply NAME parameters from dim:frame-parameters to FRAME"
  (destructuring-bind ((top left) (cols rows))
      (rest (assoc name dim:frame-parameters))
    (set-frame-parameter frame 'font "Andale Mono-14")
    (set-frame-position frame top left)
    (set-frame-size frame cols rows)))

(defun dim:frame-iconified-p (frame)
  (eq (frame-visible-p frame) 'icon))

(defun dim:adapt-frames-to-screen-dimensions ()
  "Setup the screen frames"
  (interactive)
  (let* ((frames (remove-if #'dim:frame-iconified-p (frame-list)))
	 (total  (length (frame-list)))
	 (nbfrms (length frames)))
    (cond ((and (eq 1 nbfrms) (eq 1 total))
	   ;; start from single default frame
	   (if (equal (get-screen-dimensions) '(2560 1440))
	       (progn
		 (dim:set-frame-parameters (selected-frame) 'left)
		 (let ((right-frame (make-frame)))
		   (dim:set-frame-parameters right-frame 'right)))
	     (dim:set-frame-parameters (selected-frame) 'single)))

	  ((and (eq 1 nbfrms) (< 1 total))
	   ;; hidden frames means we want the 'single parameter
	   (dim:set-frame-parameters (selected-frame) 'single))

	  ((eq 2 nbfrms)
	   ;; likely the 2 frames already do exists
	   (loop for f in frames
		 do (loop for (name . ((top left) (cols rows)))
			  in dim:frame-parameters
			  when (eq cols (frame-parameter f 'width))
			  do (dim:set-frame-parameters f name))))

	  (t (error "What the fuck?")))))

(defun dim:set-frame-size (&optional name)
  "Ask for for the frame size setup name to use then apply it"
  (interactive
   (list
    (completing-read "Resize frame as: " '("left" "right" "single") nil t)))
  (let ((name (if (symbolp name) name (intern name))))
    (dim:set-frame-parameters (selected-frame) name)))

(global-set-key (kbd "C-M-`") 'dim:adapt-frames-to-screen-dimensions)

;; some special for offlineimap
;; (require 'dim-offlineimap)
;; (setq dim:offlineimap-bin "/sw/bin/offlineimap"
;;       dim:offlineimap-options "")

;; home only usage
(require 'betaseries)

;; talk to iTunes
(defun itunes-insert-current-song ()
  "return, message or insert current track information from iTunes"
  (interactive "")
  (let ((current-song
	 (car (split-string
	       (shell-command-to-string
		(concat "osascript -e "
			"'tell application \"iTunes\" to return "
			"the artist of current track "
			"& \" - \" "
			"& the album of current track"
			"& \" - \" "
			"& the name of current track'")) "\n"))))
    (insert (format "%s" current-song))
    current-song))

(provide 'dim-init-macosx)
