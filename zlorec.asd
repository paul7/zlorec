(defsystem #:zlorec
  :depends-on (#:restas #:closure-template #:postmodern 
	       #:drakma #:cl-ppcre #:local-time)
  :components ((:module "src"
			:components ((:file "defmodule")
				     (:file "xonix")
				     (:file "user-activity"
					    :depends-on ("xonix"))
				     (:file "graph"
					    :depends-on ("defmodule"
							 "user-activity"))))))
