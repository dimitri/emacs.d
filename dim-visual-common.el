;;; dim-visual-common.el --- Dimitri Fontaine

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(scroll-bar-mode nil)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

(line-number-mode 1)
(column-number-mode 1)
(setq-default fill-column 76)
(setq auto-fill-mode 1)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; we want global-hl-line-mode except in M-x term buffers.
(global-hl-line-mode 1)

(defadvice global-hl-line-highlight
  (around dim:global-hl-line-highlight activate)
  (unless (and (eq major-mode 'term-mode) (term-in-char-mode)) ad-do-it))

(transient-mark-mode 1)

;; quite a stretch, but has its place here too
(setq indent-tabs-mode nil)

(require 'font-lock)
(global-font-lock-mode 1)

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

  ;; other systems are yet unknown
  (t '(0 0))))

(provide 'dim-visual-common)
