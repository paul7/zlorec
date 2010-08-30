(restas:define-module #:zlorec
    (:use #:cl #:postmodern #:drakma #:cl-ppcre #:local-time #:parse-html)
  (:export))

(in-package #:zlorec)

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
