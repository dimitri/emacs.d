;;; dim-custom.el
;;
;; sometimes Emacs does it all by itself.

(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "e5596ad72909f3a0549745439482366daef4ec33")
 '(custom-safe-themes
   '("a5f6d6d1b36441c0249e042a715bd3a286ca92df3a31844beaa5bdceac72ce6d" default))
 '(ldap-host-parameters-alist nil)
 '(package-selected-packages '(magit))
 '(safe-local-variable-values
   '((Lowercase . T)
     (Package ANSI-LOOP "COMMON-LISP")
     (Package . bind)
     (external-format . utf-8)
     (Package . metabang.graph)
     (Package RT :use "COMMON-LISP" :colon-mode :external)
     (syntax . COMMON-LISP)
     (Package . CL-FAD)
     (Package . CL-WHO)
     (Syntax . Common-lisp)
     (package . cl-user)
     (Package . COMMON-LISP-USER)
     (Package . HUNCHENTOOT)
     (Encoding . utf-8)
     (readtable . runes)
     (Package . SAX)
     (whitespace-style face tabs trailing lines-tail)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook 'write-contents-functions
                     (lambda nil
                       (delete-trailing-whitespace)
                       nil))
           (require 'whitespace)
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (Package . getopt)
     (Package . CCL)
     (Syntax . Common-Lisp)
     (Package . CL-USER)
     (Package . CL-UNICODE)
     (Syntax . ANSI-Common-Lisp)
     (Package . CL-PPCRE-TEST)
     (Base . 10)
     (Package . CL-PPCRE)
     (Syntax . COMMON-LISP)
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook 'write-contents-functions
                     (lambda nil
                       (delete-trailing-whitespace
                        (point-min)
                        (point-max))
                       nil))
           (require 'whitespace)
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (eval ignore-errors
           (add-hook 'before-save-hook 'delete-trailing-whitespace)
           (require 'whitespace)
           (set-face-attribute 'whitespace-line nil :background "red1" :foreground "yellow" :weight 'bold)
           (set-face-attribute 'whitespace-tab nil :background "red1" :foreground "yellow" :weight 'bold)
           "Need to ensure that whitespace mode is turned off and on again. This guaranteees that the new values of the whitespace-related variables will take effect."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (eval progn
           (add-hook 'before-save-hook 'delete-trailing-whitespace)
           (set-face-attribute 'whitespace-line nil :background "red1" :foreground "yellow" :weight 'bold)
           (set-face-attribute 'whitespace-tab nil :background "red1" :foreground "yellow" :weight 'bold)
           (require 'whitespace)
           "Need to ensure that whitespace mode is turned off and on again. This guaranteees that the new values of the whitespace-related variables will take effect."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (require-final-newline . t)
     (eval set-face-attribute 'whitespace-line nil :background "red1" :foreground "yellow" :weight 'bold)
     (eval set-face-attribute 'whitespace-tab nil :background "red1" :foreground "yellow" :weight 'bold)
     (whitespace-style face trailing lines-tail)
     (whitespace-line-column . 80)
     (eval require 'whitespace)
     (show-trailing-whitespace . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)) t)
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) nil)) t)
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background dark)) nil)) t)
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background dark)) nil)) t)
 '(mumamo-background-chunk-submode4 ((((class color) (min-colors 88) (background dark)) nil)) t))
