;;; dim-org.el --- Dimitri Fontaine

(require 'org-install)

(setq org-agenda-files '("~/2ndQuadrant/Gestion/Calendrier")
      org-completion-use-ido t
      org-return-follows-link t)

(global-set-key (kbd "C-c a") 'org-aagenda)

(provide 'dim-org)
