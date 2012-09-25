;;; init-multiple-cursors.el --- el-get init file for package multiple-cursors
;;

(global-set-key (kbd "C-M-<") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M->") 'mc/mark-all-like-this)
