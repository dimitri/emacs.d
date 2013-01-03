;;; dim-org.el --- Dimitri Fontaine

(require 'org-install)

;; http://orgmode.org/manual/Conflicts.html#Conflicts
(setq org-replace-disputed-keys t)

;; Make windmove work in org-mode:
(progn
  (add-hook 'org-metaup-hook 'windmove-up)
  (add-hook 'org-metadown-hook 'windmove-down)
  (add-hook 'org-metaleft-hook 'windmove-left)
  (add-hook 'org-metaright-hook 'windmove-right))

(setq org-agenda-files '("~/2ndQuadrant/Gestion/Calendrier")
      org-completion-use-ido t
      org-return-follows-link t)

(global-set-key (kbd "C-c a") 'org-aagenda)

(provide 'dim-org)
