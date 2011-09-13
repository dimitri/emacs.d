;;; init-asciidoc.el --- el-get init file for package asciidoc
;;

(autoload 'doc-mode "doc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.adoc$" . doc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . doc-mode))
(add-hook 'doc-mode-hook
	  '(lambda ()
	     (turn-on-auto-fill)
	     (require 'asciidoc)))
