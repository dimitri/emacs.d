;;; dim-custom.el
;;
;; sometimes Emacs does it all by itself.

(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (Package . getopt) (Package . CCL) (Syntax . Common-Lisp) (Package . CL-USER) (Package . CL-UNICODE) (Syntax . ANSI-Common-Lisp) (Package . CL-PPCRE-TEST) (Base . 10) (Package . CL-PPCRE) (Syntax . COMMON-LISP) (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace (point-min) (point-max)) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (eval ignore-errors (add-hook (quote before-save-hook) (quote delete-trailing-whitespace)) (require (quote whitespace)) (set-face-attribute (quote whitespace-line) nil :background "red1" :foreground "yellow" :weight (quote bold)) (set-face-attribute (quote whitespace-tab) nil :background "red1" :foreground "yellow" :weight (quote bold)) "Need to ensure that whitespace mode is turned off and on again. This guaranteees that the new values of the whitespace-related variables will take effect." (whitespace-mode 0) (whitespace-mode 1)) (eval progn (add-hook (quote before-save-hook) (quote delete-trailing-whitespace)) (set-face-attribute (quote whitespace-line) nil :background "red1" :foreground "yellow" :weight (quote bold)) (set-face-attribute (quote whitespace-tab) nil :background "red1" :foreground "yellow" :weight (quote bold)) (require (quote whitespace)) "Need to ensure that whitespace mode is turned off and on again. This guaranteees that the new values of the whitespace-related variables will take effect." (whitespace-mode 0) (whitespace-mode 1)) (require-final-newline . t) (eval set-face-attribute (quote whitespace-line) nil :background "red1" :foreground "yellow" :weight (quote bold)) (eval set-face-attribute (quote whitespace-tab) nil :background "red1" :foreground "yellow" :weight (quote bold)) (whitespace-style face trailing lines-tail) (whitespace-line-column . 80) (eval require (quote whitespace)) (show-trailing-whitespace . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode4 ((((class color) (min-colors 88) (background dark)) nil))))
