;;; init-pandoc-mode.el --- el-get init file for package pandoc-mode
;;

(setq pandoc-binary (executable-find "pandoc"))
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'rst-mode-hook 'turn-on-pandoc)
