;;; dim-lib.el --- Dimitri Fontaine
;;
;; Elisp does not have it all, so here additional lib stuff

;; I don't like the clever while hack to have until, let's hide it
(defmacro until (cond &rest body) `(while (progn ,@body ,cond)))

(provide 'dim-lib)