;;; init-offlineimap.el --- el-get init file for package offlineimap
;;

(require 'gnus-load)
(require 'gnus)
(setq offlineimap-enable-mode-line-p
      '(member major-mode '(offlineimap-mode
			    gnus-group-mode
			    gnus-summary-mode)))
(setq offlineimap-mode-line-symbol "♺")	; ♻
(setq offlineimap-timestamp "%k:%M:%S ")
(loop with color = "DarkGoldenrod"
      for face in '(offlineimap-msg-syncingfolders-face
		    offlineimap-msg-skippingfolder-face)
      do (set-face-attribute face nil :foreground color))
(define-key gnus-group-mode-map (kbd "O") 'offlineimap)
