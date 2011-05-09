;;; betaseries.el --- care about next series to download
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; Version: 0.1
;; Created: 2011-05-06
;; Keywords: emacs betaseries series transmission
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.

(require 'cl-seq)

(defvar betaseries-base-url "https://www.betaseries.com/serie/%s"
  "Base URL for betaseries")

(defvar torrentz-base-url "http://torrentz.eu/search?f="
  "Base URL for torrentz.eu")

(defcustom betaseries-alist '()
  "List of series names and id as found in the URL of betaseries,
say '((\"gameofthrones\" . \"Game Of Thrones\")) to refer to
https://www.betaseries.com/serie/gameofthrones")

;; somewhat bad style, but practical
(setq betaseries-alist '((gameofthrones  . "Game Of Thrones")
			 (vampirediaries . "Vampire Diaries")
			 (bigbangtheory  . "Big Bang Theory")
			 (trueblood      . "True Blood")
			 (breakingbad    . "Breaking Bad")))

(defun betaseries-date-in-past-p (jour mois)
  "Compute whether '(\"02\" \"mai\") is in the past"
  (let* ((fr-months
	  ["janvier" "fevrier" "mars" "avril" "mai" "juin"
	   "juillet" "aout" "septembre" "octobre" "novembre" "decembre"])
	 (j  (string-to-int jour))
	 (m  (1+ (position mois fr-months :test 'string=))))
    (time-less-p (encode-time 0 0 0 j m (nth 5 (decode-time))) (current-time))))

(defun betaseries-parse-html-buffer (buffer)
  "parse the HTML and return next air date"
  ;; (set-window-buffer (selected-window) buffer)
  (with-current-buffer buffer
    (let* ((dummy   (goto-char (point-min)))
	   (dummy   (re-search-forward "<h2>\\([^<]*\\)</h2>"))
	   (name    (match-string 1))
	   (dummy   (re-search-forward "date [^\"]+"))
	   (dummy   (re-search-backward "nb.>\\([^<]*\\)"))
	   (jour    (match-string 1))
	   (dummy   (re-search-forward "mois.>\\([^<]*\\)"))
	   (mois    (match-string 1))
	   (dummy   (re-search-forward "/episode/[^/]*/\\([^\"]*\\)"))
	   (episode (match-string 1)))
      (list episode jour mois))))

(defun betaseries-fetch-next-episode (name)
  "fetch the betaseries information for `name' series and return the buffer"
  (let* ((url     (format betaseries-base-url name))
	 (debug   (message "betaseries fetching %S" url))
	 (content (url-retrieve-synchronously url)))
    (ignore-errors (betaseries-parse-html-buffer content))))

(defun betaseries-get-torrentz-url (name episode)
  "Given a series NAME and an EPISODE, return the torrentz URL"
  (concat
   torrentz-base-url (mapconcat 'identity (split-string name) "+") "+" episode))

(defun betaseries-list-next-urls (&optional alist)
  "Run through `betaseries-alist' and return torrentz URLs for
last released episodes"
  (loop for (code . name) in (or alist betaseries-alist)
	for (episode jour mois) = (betaseries-fetch-next-episode (symbol-name code))
	do (message "betaseries %s %s" code episode)
	when (and episode (betaseries-date-in-past-p jour mois))
	collect (list name episode jour mois (betaseries-get-torrentz-url name episode))))

(defun betaseries-browse-urls (&optional alist)
  "Browse each torrent URL returned by `betaseries-list-next-urls'"
  (interactive)
  (loop for (n e j m url) in (betaseries-list-next-urls
			      (or alist betaseries-alist))
	do (browse-url url)
	collect (list n e j m url)))

