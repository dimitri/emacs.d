;;; site-start.el --- Dimitri Fontaine
;;
;; Trying to have a portable Emacs on a USB Key, containing the
;; init-user-file
;;
;; http://pigpog.com/2007/10/22/portable-emacs-onna-stick/
;;
;; Place this file in the Emacs/site-lisp folder on the Key

(defvar usb-drive-letter (substring data-directory 0 3))
(defvar usb-home-dir (concat usb-drive-letter "Home/"))

(setenv "HOME" usb-home-dir)

(setenv "PATH"
	(concat (getenv "PATH")
		";" usb-drive-letter "Emacs/emacs-24.0.50/bin/"
		";" usb-drive-letter "Git/bin/"
		";" usb-drive-letter "Cygwin/CygwinPortable/App/Cygwin/bin/"
		";" usb-drive-letter "usr/bin/"))

(add-to-list 'exec-path (concat usb-drive-letter "Emacs/emacs-24.0.50/bin/"))
(add-to-list 'exec-path (concat usb-drive-letter "Git/bin/"))
(add-to-list 'exec-path (concat usb-drive-letter "Cygwin/CygwinPortable/App/Cygwin/bin/"))
(add-to-list 'exec-path (concat usb-drive-letter "usr/bin/"))
