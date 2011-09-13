;;; init-emms.el --- el-get init file for package emms
;;

(emms-standard)

;; (emms-default-players) ; I want VLC mainly
(setq emms-player-list
      '(emms-player-vlc
	emms-player-mpg321
	emms-player-ogg123
	emms-player-mplayer-playlist
	emms-player-mplayer))

;; M-x emms-smart-browse
(require 'emms-browser)
(require 'emms-source-file-directory-tree-find)
(setq emms-source-file-directory-tree-function
      'emms-source-file-directory-tree-find)

(setq emms-source-file-default-directory "~/Music/")

;; some memory of what we did
(require 'emms-history)
(emms-history-load)

(global-set-key (kbd "<f9>") 'emms-smart-browse)
(define-key emms-browser-mode-map (kbd "+") 'emms-volume-raise)
(define-key emms-browser-mode-map (kbd "-") 'emms-volume-lower)

;; Show the current track each time EMMS
;; starts to play a track with "NP : "
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "EMMS Now Playing: %s")

(add-hook 'dired-load-hook
	  (define-key dired-mode-map (kbd "E") 'emms-play-dired))
