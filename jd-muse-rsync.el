;;; jd-muse-rsync.el --- Julien Danjou
;; Thanks jd!

(require 'muse-project)

(defcustom jd:muse-rsync-options "-avz"
  "Options to give to the rsync process.")

(defcustom jd:muse-rsync-buffer-name "*muse-project-rsync*"
  "Buffer name for the rsync output. If nil, does not print result.")

(defun jd:muse-project-rsync (&optional silent)
  "Publish a muse project using rsync. The project muse
have :path (source) and :rsync-target (destination) set."
  (interactive "P")
  (let ((project (muse-project)))
    (unless project
      (error "Project not found."))
    (let* ((project-prop (caddr project))
           (project-source (plist-get project-prop :path))
           (project-target (plist-get project-prop :rsync-target)))
      (unless project-source
        (error "Project has no source."))
      (unless project-target
        (error "Project has no target."))
      (let ((rsync-command (format "rsync %s %s/ %s/"
                                   jd:muse-rsync-options
                                   project-source
                                   project-target))
            (rsync-buffer (and jd:muse-rsync-buffer-name
                               (not silent)
                               (get-buffer-create jd:muse-rsync-buffer-name))))
        (when rsync-buffer
          (set-buffer rsync-buffer)
          (erase-buffer)
          (insert (concat rsync-command "\n"))
          (display-buffer rsync-buffer))
        (call-process-shell-command rsync-command
                                    nil
                                    rsync-buffer)))))

(provide 'jd-muse-rsync)
