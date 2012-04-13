;;; dim-lisp.el --- Dimitri Fontaine
;;
;; Emacs and Lisp is SLIME, Quicklisp is the easy way here.

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/Users/dim/dev/CL/ccl/dx86cl64 -K utf-8")

(provide 'dim-lisp)
