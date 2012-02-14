;;; init-popwin.el --- el-get init file for package popwin
;;

(loop for b in '("*Ido Completions*"
		 "*Quail Completions*"
		 "*Anything Occur*"
		 "*Disabled Command*"
		 "*Backtrace*"
		 "*Kill Ring*")
      do (add-to-list
	  'popwin:special-display-config
	  `(,b :noselect t))
      finally return popwin:special-display-config)
