;;; dim-keys.el --- some additional shortcuts

;; C-c r pour revert-buffer
(global-set-key (kbd "C-c r") '(lambda () (interactive) (revert-buffer)))

;; C-c d pour écrire la date
(defun insert-date()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%Y%m%d" (current-time))))

(global-set-key "\C-cd" 'insert-date)

;; C-c C-t prefix numéros de tel
(defun dim:dim-numtel-pro()
  "Insert my numtel"
  (interactive)
  (insert "01 73 03 42 31"))

(defun dim:sebl-numtel-pro()
  "Insert my numtel"
  (interactive)
  (insert "02 28 08 20 71"))

(defun dim:cbouthors-numtel-pro()
  "Insert my numtel"
  (interactive)
  (insert "02 28 08 20 74"))

(global-set-key (kbd "C-c ! d") 'dim:dim-numtel-pro)
(global-set-key (kbd "C-c ! s") 'dim:sebl-numtel-pro)
(global-set-key (kbd "C-c ! c") 'dim:cbouthors-numtel-pro)

(provide 'dim-keys)