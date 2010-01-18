;;; dim-visual-common.el --- Dimitri Fontaine

(setq inhibit-startup-message t)
(menu-bar-mode nil)
(scroll-bar-mode nil)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

(line-number-mode 1)
(column-number-mode 1)
(setq-default fill-column 76)
(setq auto-fill-mode 1)

(global-hl-line-mode 1)
(transient-mark-mode 1)

(require 'font-lock)
(global-font-lock-mode 1)

(require 'woman)
(setq woman-use-own-frame nil)

(defun get-screen-dimensions ()
  "get dimensions of current screen, support X window system and macosx"

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

  ;; linux version is yet to write
  (t '(0 0))))

(provide 'dim-visual-common)
