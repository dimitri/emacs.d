;;; init-anything.el --- el-get init file for package anything
;;

;; we redefine anything-occur to allow for match plugin
(defun dim:anything-occur ()
  "Preconfigured Anything for Occur source.
If region is active, search only in region,
otherwise search in whole buffer."
  (interactive)
  (save-restriction
    (when (region-active-p)
      (narrow-to-region (region-beginning) (region-end)))
    (anything-other-buffer 'anything-c-source-occur "*Anything Occur*")))
