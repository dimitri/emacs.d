;;; init-dictionary.el --- el-get init file for package dictionary
;;

(require 'dictionary)

;; (make-local-hook HOOK)
;; This function is obsolete since 21.1;
;; not necessary any more.
(defun make-local-hook (name) "backward compat" nil)

(setq dictionary-coding-systems-for-dictionaries '(("robert" . iso-8859-15)))
(unless (string-match "apple-darwin" system-configuration)
  (setq dictionary-server "localhost"))
