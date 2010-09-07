(defsystem #:zlorec
  :depends-on (#:restas #:closure-template #:postmodern 
	       #:drakma #:cl-ppcre #:cl-postgres+local-time #:parse-html
	       #:lweb)
  :components ((:module "src"
			:components ((:file "defmodule")
				     (:file "xonix"
					    :depends-on ("defmodule"))
				     (:file "retriever"
					    :depends-on ("defmodule"))
				     (:file "local-stats"
					    :depends-on ("retriever"))
				     (:file "user-activity"
					    :depends-on ("xonix" 
							 "local-stats"))
				     (:file "graph"
					    :depends-on ("defmodule"
							 "user-activity"))))))
