(defsystem #:zlorec
  :depends-on (#:restas #:closure-template #:postmodern 
	       #:drakma #:cl-ppcre #:cl-postgres+local-time #:parse-html
	       #:lweb #:persistent-memoize+restas #:persistent-memoize+postmodern)
  :components ((:module "src"
			:components ((:file "defmodule")
				     (:file "message"
					    :depends-on ("defmodule"))
				     (:file "aux"
					    :depends-on ("message"))
				     (:file "xonix"
					    :depends-on ("defmodule"))
				     (:file "retriever"
					    :depends-on ("message"))
				     (:file "local-stats"
					    :depends-on ("retriever"))
				     (:file "user-activity"
					    :depends-on ("xonix" 
							 "local-stats"))
				     (:file "graph"
					    :depends-on ("defmodule"
							 "aux"
							 "user-activity"))))))
