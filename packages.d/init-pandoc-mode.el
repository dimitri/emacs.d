;;; init-pandoc-mode.el --- el-get init file for package pandoc-mode
;;

(setq pandoc-binary (executable-find "pandoc"))
(require 'pandoc-mode)
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'rst-mode-hook 'turn-on-pandoc)
