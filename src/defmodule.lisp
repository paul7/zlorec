(restas:define-module #:zlorec
    (:use #:cl #:postmodern #:drakma #:cl-ppcre #:local-time #:parse-html #:iterate #:lweb)
  (:export))

(in-package #:zlorec)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun recompile-templates ()
    (closure-template:compile-template :common-lisp-backend
				       (merge-pathnames "src/graph.tmpl"
							(asdf:component-pathname (asdf:find-system '#:zlorec))))
    (closure-template:compile-template :common-lisp-backend
				       (merge-pathnames "src/lweb-subview.tmpl"
							(asdf:component-pathname (asdf:find-system '#:zlorec))))
    (values))

  (recompile-templates))

(defun render-method-for-package (pkg)
  #'(lambda (obj)
      (closure-template.standard:xhtml-strict-frame
       (list :title (getf obj :title)
	     :body (zlorec.view:main-view 
		    (list
		     :impl (list :type (lisp-implementation-type)
				 :version (lisp-implementation-version))
		     :body (restas:render-object 
			    (find-package pkg)
			    obj)))))))

(restas:define-submodule #:lweb-viewer (#:lweb)
  (*default-render-method* (render-method-for-package ':lweb.subview))
  (lweb::*db-spec* '("zlodb" "lisp" "lisp" "localhost"))
  (lweb::*message-class* 'zlorec::message))

(defun image-path (name)
  (merge-pathnames name 
		   (merge-pathnames "img/"
				    (asdf:component-pathname (asdf:find-system '#:zlorec)))))
