;;; init-smex.el --- el-get init file for package smex
;;

(setq smex-save-file "~/.emacs.d/.smex-items")
(global-set-key (kbd "ESC M-x") 'execute-extended-command)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
