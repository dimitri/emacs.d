;; Custom window splitting shortcuts
;;  C-c 1  horizontal split, very little window on the bottom
;;  C-c 2  horizontal split, 3/4 of the space atop, 1/4 at the bottom
;;  C-c 3  vertical splitting of main window, right part horizontally split

(defun split-window-vertically-min-bottom ()
  "split current window vertically and select new window, 4 lines height"
  (interactive)
  (select-window (split-window-vertically -4)))

(defun split-window-vertically-quarter-bottom ()
  "split current window vertically and select new window, 1/4 of current window height"
  (interactive)
  (let* ((edges  (window-edges))
	 (top    (second edges))
	 (bottom (fourth edges))
	 (heigth (- bottom top)))
    (select-window (split-window-vertically (- (/ heigth 4))))))

(defun split-window-in-three ()
  "split current window horizontally then split new window vertically"
  (interactive)
  (select-window (split-window-horizontally 90))
  (split-window-vertically))

(defun split-window-for-rcirc ()
  "split current window horizontally then split left part in three, vertically"
  (interactive)
  (split-window-horizontally)

  (split-window-vertically)
  (split-window-vertically)
  (balance-windows))

(global-set-key (kbd "C-c 1") 'split-window-vertically-min-bottom)
(global-set-key (kbd "C-c 2") 'split-window-vertically-quarter-bottom)
(global-set-key (kbd "C-c 3") 'split-window-in-three)
(global-set-key (kbd "C-c 4") 'split-window-for-rcirc)

;; Add specific key mapping to term mode, as C-c 1 is already in use
(require 'term)
(define-key term-raw-map (kbd "C-c c 1") 'split-window-vertically-min-bottom)
(define-key term-raw-map (kbd "C-c c 2") 'split-window-vertically-quarter-bottom)
(define-key term-raw-map (kbd "C-c c 3") 'split-window-in-three)
(define-key term-raw-map (kbd "C-c c 4") 'split-window-for-rcirc)

;;
;; C-c t splits current window and open a new term
;;
(defun split-horizontally-and-open-a-term (&optional split-term-buffer-name)
  "split horizontaly, open a term on the right wuth bash inside"
  "Opens a M-x term and type in ssh remotehost with given hostname"
  (interactive
   (list
    (and current-prefix-arg
	 (read-buffer "Term buffer name: "
		      (generate-new-buffer-name "terminal")))))
  (setq split-term-buffer-name
	(or split-term-buffer-name "terminal"))

  (select-window (split-window-horizontally))
  (unless (get-buffer split-term-buffer-name)
    (ansi-term "/bin/bash" split-term-buffer-name)))

(global-set-key (kbd "C-c t") 'split-horizontally-and-open-a-term)

(provide 'dim-splits)