(restas:define-module #:zlorec
    (:use #:cl #:postmodern #:drakma #:cl-ppcre #:local-time #:parse-html #:iterate #:lweb)
  (:export))

(in-package #:zlorec)

(restas:define-submodule #:lweb-viewer (#:lweb)
  (*default-render-method* #'(lambda (obj)
			      (closure-template.standard:xhtml-strict-frame
			       (list :title (getf obj :title)
				     :body (lweb.view:main-view 
					    (list 
					     :login (restas:genurl 'login-form 
								   :return (hunchentoot:url-encode (hunchentoot:request-uri*)))
					     :impl (list :type (lisp-implementation-type)
							 :version (lisp-implementation-version))
					     :index (restas:genurl 'message-list)
					     :body (restas:render-object 
						    (find-package ':lweb.view)
						    obj))))))))


(defun recompile-templates ()
  (closure-template:compile-template :common-lisp-backend
				     (merge-pathnames "src/graph.tmpl"
						      (asdf:component-pathname (asdf:find-system '#:zlorec))))
  (values))

(recompile-templates)

(defun image-path (name)
  (merge-pathnames name 
		   (merge-pathnames "img/"
				    (asdf:component-pathname (asdf:find-system '#:zlorec)))))
