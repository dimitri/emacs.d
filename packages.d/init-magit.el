;;; init-magit.el --- el-get init file for package magit
;;

;; we use C-x C-z as the magit access key
(global-set-key (kbd "C-x C-z") 'magit-status)

;; C-c C-a to amend without any prompt
(defun magit-just-amend ()
  (interactive)
  (save-window-excursion
    (magit-with-refresh
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))

(eval-after-load "magit"
  '(define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend))
