;;; init-git-gutter.el --- el-get init file for package git-gutter
;;

(require 'git-gutter)

;; from https://github.com/syohex/emacs-git-gutter
(setq git-gutter:lighter " GG")

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-diff)

;; Jump to next/previous diff
(global-set-key (kbd "C-x g p") 'git-gutter:previous-diff)
(global-set-key (kbd "C-x g n") 'git-gutter:next-diff)
