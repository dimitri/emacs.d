;;; init-smartparens.el --- el-get init file for package smartparens

(require 'smartparens-config)
(smartparens-global-strict-mode 1)
(smartparens-global-mode 1)

;; manually remove that major annoyance.
(define-key smartparens-mode-map (kbd "M-<up>") 'windmove-up)
(define-key smartparens-mode-map (kbd "M-<down>") 'windmove-down)

