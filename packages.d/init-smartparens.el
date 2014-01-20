;;; init-smartparens.el --- el-get init file for package smartparens

(require 'smartparens)

;; (smartparens-global-strict-mode 1)
;; (smartparens-global-mode -1)

(sp-pair "'" nil :unless '(sp-point-after-word-p))

(--each sp--html-modes
  (eval-after-load (symbol-name it) '(require 'smartparens-html)))
(eval-after-load "latex"         '(require 'smartparens-latex))
(eval-after-load "tex-mode"      '(require 'smartparens-latex))
(eval-after-load "lua-mode"      '(require 'smartparens-lua))
(eval-after-load "ruby-mode"     '(require 'smartparens-ruby))
(eval-after-load "enh-ruby-mode" '(require 'smartparens-ruby))

;; manually remove that major annoyance.
(define-key smartparens-mode-map (kbd "M-<up>") 'windmove-up)
(define-key smartparens-mode-map (kbd "M-<down>") 'windmove-down)

