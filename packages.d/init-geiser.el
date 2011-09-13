;;; init-geiser.el --- el-get init file for package geiser
;;

(setq geiser-guile-binary "guile-2.0")
(setq geiser-repl-use-other-window nil)
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")
(setq geiser-active-implementations '(guile))
