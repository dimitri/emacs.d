;;; init-paredit.el --- el-get init file for package paredit

(require 'paredit)

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

;; manually remove that major annoyance.
(define-key paredit-mode-map (kbd "M-<up>") 'windmove-up)
(define-key paredit-mode-map (kbd "M-<down>") 'windmove-down)
