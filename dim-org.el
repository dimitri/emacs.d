;;; dim-org.el --- Dimitri Fontaine
;;
;; Based on http://doc.norang.ca/org-mode.html

;(require 'org)
(require 'org-install)

(setq org-agenda-files
      ;; put Hi-Media files first, they're not existing @home
      ;; so won't get choosen for refiling there.
      (loop for path in '("~/Hi-Media/hi-media.org"
			  "~/Hi-Media/refile.org"
			  "~/Quadrant/quadrant.org"
			  "~/Quadrant/refile.org"
			  "~/dev/emacs.d/org/pgsql.org")
	    when (file-exists-p (expand-file-name path))
	    collect path))

(setq
 org-todo-keywords
 '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
   (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "|" "CANCELLED(c@/!)")))

(setq org-use-fast-todo-selection t)
;(setq org-treat-S-cursor-todo-selection-as-state-change t)

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("STARTED" :foreground "blue" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("SOMEDAY" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)))

 ;; ("QUOTE" :foreground "red" :weight bold)
 ;; ("QUOTED" :foreground "magenta" :weight bold)
 ;; ("APPROVED" :foreground "forest green" :weight bold)
 ;; ("EXPIRED" :foreground "forest green" :weight bold)
 ;; ("REJECTED" :foreground "forest green" :weight bold)
 ;; ("OPEN" :foreground "blue" :weight bold))))

(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAITING" ("WAITING" . t) ("NEXT"))
        ("SOMEDAY" ("WAITING" . t))
        (done ("NEXT") ("WAITING"))
        ("TODO" ("WAITING") ("CANCELLED") ("NEXT"))
        ("STARTED" ("WAITING"))
        ("DONE" ("WAITING") ("CANCELLED") ("NEXT"))))

;;;
;; Tags with fast selection keys
(setq org-tag-alist '((:startgroup)
		      ("@anywhere" . ?a)
		      ("@home"     . ?h)
		      ("@evening"  . ?e)
		      ("@himedia"  . ?f)
		      (:endgroup)
		      ("TEL" . ?T)
		      ("DEVIS" . ?D)
		      ("FACTURE" . ?F)
		      ("CLIENT" . ?C)
		      ("PROJET" . ?P)
		      ("WAITING" . ?w)
		      ("ADMIN" . ?A)
		      ("NOTE" . ?n)
		      ("Cedric" . ?c)
		      ("CANCELLED" . ?X)))

;;;
;; REMEMBER
;;
;; refile into the first refile.org found in org-agenda-files
(setq org-default-notes-file
      (loop for orgfile in org-agenda-files
	    when (string= (file-name-nondirectory orgfile) "refile.org")
	    return orgfile))

(require 'remember)
(org-remember-insinuate)
(global-set-key (kbd "C-S-r") 'org-remember)

(setq
 org-remember-templates
 '(("todo" ?t "* TODO %?\n  %u\n  %a" nil bottom nil)
   ("note" ?n "* %?                                        :NOTE:\n  :CLOCK:\n  :END:\n  %U\n  %a" nil bottom nil)
   ("org-protocol" ?w "* TODO Review %c%!\n  %U" nil bottom nil)))

;;;
;; REFILING
; Use IDO for target completion
(setq org-completion-use-ido t)

; Targets include this file and any file contributing to the agenda - up to
; 5 levels deep
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 5)
        (nil :maxlevel . 5)))

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path 'file)

; Targets complete in steps so we start with filename, TAB shows the next
; level of targets etc
(setq org-outline-path-complete-in-steps t)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;;;
;; AGENDA

(setq org-agenda-custom-commands
      '(("s" "Started Tasks" todo "STARTED"
         ((org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)))

        ("w" "Tasks waiting on something" tags "WAITING/!"
         ((org-use-tag-inheritance nil)))

        ("r" "Refile New Notes and Tasks" tags "LEVEL=1+REFILE"
         ((org-agenda-todo-ignore-with-date nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-scheduled nil)))

        ("N" "Notes" tags "NOTE" nil)
        ("n" "Next" tags "NEXT-WAITING-CANCELLED/!" nil)
        ("p" "Projects" tags-todo "LEVEL=2-NEXT-WAITING-CANCELLED/!-DONE" nil)
        ("A" "Tasks to be Archived" tags "LEVEL=2/DONE|CANCELLED" nil)

        ("h" "Habits" tags "STYLE=\"habit\""
         ((org-agenda-todo-ignore-with-date nil)
          (org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)))))

;;;
;; LOGGING
(setq org-log-done 'note)
(setq org-log-into-drawer t)

;;;
;; CLOCKING

(org-clock-persistence-insinuate)
(setq org-clock-in-switch-to-state "STARTED")
(setq org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK"))
(setq org-clock-into-drawer "CLOCK")
(setq org-clock-persist 'history)
(setq org-time-stamp-rounding-minutes '(1 10))

;; Change task state to STARTED from TODO when clocking in
(defun bh/clock-in-to-started (kw)
  "Switch task from TODO to STARTED when clocking in"
  (if (and (string-equal kw "TODO")
           (not (string-equal (buffer-name) "*Remember*")))
      "STARTED"
    nil))

;(setq org-clock-in-switch-to-state 'bh/clock-in-to-started)

;;;
;; AGENDA - logging and clocking in there
;(setq org-agenda-log-mode-items '(clock))

;;;
;; EFFORT / Column View
; global Effort estimate values
(setq org-global-properties
      '(("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")))

(setq org-columns-default-format
      "%30ITEM %10SCHEDULED %TODO %3PRIORITY %TAGS")
;     "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

;;;
;; KEYS
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-return-follows-link t)

;;;
;; Do not open new frames when following link
(setq org-link-frame-setup
      '((gnus . org-gnus-no-new-news)
        (file . find-file)))

(provide 'dim-org)
