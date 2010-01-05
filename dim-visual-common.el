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

(provide 'dim-visual-common)
