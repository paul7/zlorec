(in-package #:zlorec)

(setf *default-render-method*
      #'(lambda (obj)
	  (closure-template.standard:xhtml-strict-frame
	   (list :title (getf obj :title)
		 :body (zlorec.view:main-view 
			(list 
			 :impl (list :type (lisp-implementation-type)
				     :version (lisp-implementation-version))
			 :body (restas:render-object 
				(find-package ':zlorec.view)
				obj)))))))

(restas:define-route main ("")
  (restas:redirect 'graph))

(restas:define-route graph ("graph")
  (list :graph (restas:genurl 'user-activity)))

(restas:define-route user-activity ("useract.svg"
				    :render-method #'zlorec.view:user-activity
				    :content-type "image/svg+xml"))
