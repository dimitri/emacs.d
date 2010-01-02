;;; dim-tels-pro.el --- Dimitri Fontaine
;;
;; I should have a proper look at BBDB

;; C-c C-t prefix numéros de tel
(defun dim:dim-numtel-pro()
  "Insert my numtel"
  (interactive)
  (insert "01 73 03 42 31"))

(defun dim:fmechineau-numtel-pro()
  "Insert my numtel"
  (interactive)
  (insert "01 73 03 89 43"))

(defun dim:sebl-numtel-pro()
  "Insert my numtel"
  (interactive)
  (insert "02 28 08 20 71"))

(defun dim:cbouthors-numtel-pro()
  "Insert my numtel"
  (interactive)
  (insert "02 28 08 20 74"))

(global-set-key (kbd "C-c ! d") 'dim:dim-numtel-pro)
(global-set-key (kbd "C-c ! f") 'dim:fmechineau-numtel-pro)
(global-set-key (kbd "C-c ! s") 'dim:sebl-numtel-pro)
(global-set-key (kbd "C-c ! c") 'dim:cbouthors-numtel-pro)

(provide 'dim-tels-pro)