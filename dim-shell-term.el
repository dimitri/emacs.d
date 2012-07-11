;;; dim-shell-term.el --- Dimitri Fontaine
;;

(require 'shell)
(require 'term)

;; M-x shell
(defun dim:get-git-root ()
  "return the top level directory where current git sources are held"
  (let* ((git (executable-find "git"))
	 (cdup
	  (ignore-errors
	    (car (process-lines git "rev-parse" "--show-cdup")))))
    (message "%S" cdup)
    (if (and cdup (not (string-match "^fatal" cdup)))
	(file-name-as-directory (expand-file-name cdup))
      default-directory)))

(defun cw:shell:run ()
  "Run shell in `default-directory' and set buffer name."
  (interactive)
  (let ((default-directory (or (dim:get-git-root) default-directory)))
    (shell (format "* Shell: %s *" default-directory))))

(global-set-key (kbd "C-M-'") 'cw:shell:run)

;; pour les couleurs dans M-x shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; M-x term
(setq term-default-bg-color (frame-parameter nil 'background-color))
(setq term-default-fg-color (frame-parameter nil 'foreground-color))
(add-hook 'term-mode-hook (lambda () (setq truncate-lines t)))

;; Toogle between line and char mode in term-mode
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
(define-key term-raw-map  (kbd "C-y") 'term-paste)

;; allow easy switching from shell to term and back
(defun term-switch-to-shell-mode ()
  (interactive)
  (if (equal major-mode 'term-mode)
      (progn
        (shell-mode)
        (set-process-filter  (get-buffer-process (current-buffer)) 'comint-output-filter )
        (local-set-key (kbd "C-M-'") 'term-switch-to-shell-mode)
        (compilation-shell-minor-mode 1)
        (comint-send-input))
    (progn
      (compilation-shell-minor-mode -1)
      (font-lock-mode -1)
      (set-process-filter  (get-buffer-process (current-buffer)) 'term-emulate-terminal)
      (term-mode)
      (term-char-mode)
      (term-send-raw-string (kbd "C-l")))))

(define-key term-raw-map (kbd "C-M-'") 'term-switch-to-shell-mode)

(provide 'dim-shell-term)
