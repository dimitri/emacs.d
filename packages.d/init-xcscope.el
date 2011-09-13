;;; init-xcscope.el --- el-get init file for package xcscope
;;
;; cscope debian package includes cscope-indexer, no luck under MacOSX

(setq cscope-indexing-script
      (if (file-executable-p "/usr/bin/cscope-indexer")
	  "/usr/bin/cscope-indexer"
	"~/bin/cscope-indexer"))
