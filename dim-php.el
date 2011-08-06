;;; dim-php.el --- Dimitri Fontaine
;;
;; add a php-lint command, and bind it to

(defun dim:php-lint ()
  "Run php -l on the current buffer in a *Compile* buffer"
  (interactive)
  (compile (format "php -l %s" (buffer-file-name))))

;; we're using php-mode-improved, steal one of its chords
(define-key php-mode-map (kbd "C-c C-L") 'c-toggle-electric-state)
(define-key php-mode-map (kbd "C-c C-l") 'dim:php-lint)

(provide 'dim-php)
