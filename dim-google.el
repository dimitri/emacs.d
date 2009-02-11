;;; dim-google.el
;;
;; Ask google about current region, select word-at-point if no region is
;; selected
;;
;; Allow user to confirm search keywords

(require 'browse-url)

(defun dim:google (min max)
  "Form a google query URL and give it to rcirc-browse-url"
  (interactive "r")
  (browse-url 
   (concat "http://www.google.com/search?q=" 
	   (replace-regexp-in-string 
	    "[[:space:]]+"
	    "+"
	    (buffer-substring min max)))))

(global-set-key (kbd "C-c g") 'dim:google)

(provide 'dim-google)
