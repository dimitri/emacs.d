;;; dim-escreen.el --- Dimitri Fontaine
;; escreen from http://www.splode.com/~friedman/software/emacs-lisp/
(load "escreen")
(escreen-install)

;; add C-\ l to list screens with emphase for current one
(defun escreen-get-active-screen-numbers-with-emphasis ()
  "what the name says"
  (interactive)
  (let ((escreens (escreen-get-active-screen-numbers))
	(emphased ""))

    (dolist (s escreens)
      (setq emphased
	    (concat emphased (if (= escreen-current-screen-number s)
				 (propertize (number-to-string s)
					     ;;'face 'custom-variable-tag) " ")
					     ;; 'face 'info-title-3)
					     'face 'font-lock-warning-face)
			       (number-to-string s))
		    " ")))
    (message "escreen: active screens: %s" emphased)))

(global-set-key (kbd "C-\\ l") 'escreen-get-active-screen-numbers-with-emphasis)

;;
;; We want the last/prev/next escreen function to show the list with
;; emphasis
;;
(defadvice escreen-goto-last-screen
  (after dim:escreen-goto-last-screen activate)
  "Show the escreen list each time we go to last screen."
  (escreen-get-active-screen-numbers-with-emphasis))

(defadvice escreen-goto-prev-screen
  (after dim:escreen-goto-prev-screen activate)
  "Show the escreen list each time we go to previous screen."
  (escreen-get-active-screen-numbers-with-emphasis))

(defadvice escreen-goto-next-screen
  (after dim:escreen-goto-next-screen activate)
  "Show the escreen list each time we go to next screen."
  (escreen-get-active-screen-numbers-with-emphasis))

(defadvice escreen-create-screen
  (after dim:escreen-create-screen activate)
  "Show the escreen list each time we create a new screen."
  (escreen-get-active-screen-numbers-with-emphasis))

;;
;; Custom escreen keys
;;
(define-key escreen-map escreen-prefix-char 'escreen-goto-last-screen)

(global-set-key (kbd "M-[") 'escreen-goto-prev-screen)
(global-set-key (kbd "M-]") 'escreen-goto-next-screen)
(global-set-key (kbd "C-\\ DEL") 'escreen-goto-prev-screen)
(global-set-key (kbd "C-\\ SPC") 'escreen-goto-next-screen)

(global-set-key '[s-mouse-4] 'escreen-goto-prev-screen)
(global-set-key '[s-mouse-5] 'escreen-goto-next-screen)

(global-set-key '[M-mouse-4] 'escreen-goto-prev-screen)
(global-set-key '[M-mouse-5] 'escreen-goto-next-screen)

;; add support for C-\ from terms
(require 'term)
(define-key term-raw-map escreen-prefix-char escreen-map)
(define-key term-raw-map (kbd "M-[") 'escreen-goto-prev-screen)
(define-key term-raw-map (kbd "M-]") 'escreen-goto-next-screen)

;; easier direct access to screens
(progn					; easier to C-M-x the block
  (global-set-key (kbd "C-M-0") 'escreen-goto-screen-0)
  (global-set-key (kbd "C-M-1") 'escreen-goto-screen-1)
  (global-set-key (kbd "C-M-2") 'escreen-goto-screen-2)
  (global-set-key (kbd "C-M-3") 'escreen-goto-screen-3)
  (global-set-key (kbd "C-M-4") 'escreen-goto-screen-4)
  (global-set-key (kbd "C-M-5") 'escreen-goto-screen-5)
  (global-set-key (kbd "C-M-6") 'escreen-goto-screen-6)
  (global-set-key (kbd "C-M-7") 'escreen-goto-screen-7)
  (global-set-key (kbd "C-M-8") 'escreen-goto-screen-8)
  (global-set-key (kbd "C-M-9") 'escreen-goto-screen-9))

(provide 'dim-escreen)
