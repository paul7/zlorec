(restas:define-module #:zlorec
    (:use #:cl #:postmodern #:drakma #:cl-ppcre #:local-time #:parse-html #:iterate #:lweb #:alexandria)
  (:export #:retrieve-loop
	   #:*wait-on-timeout*
	   #:*wait-after-block*
	   #:*block-size*
	   #:message))

(in-package #:zlorec)

(defparameter *memo-storage* (pm:make-hashtable-storage))

(restas:define-initialization (context)
  (restas:context-add-variable context 'pm:*storage* *memo-storage*))

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

(defparameter *impl-info* (list :type (lisp-implementation-type)
				:version (lisp-implementation-version)))

(defun render-method-for-package (pkg)
  #'(lambda (obj)
      (closure-template.standard:xhtml-strict-frame
       (list :title (getf obj :title)
	     :body (zlorec.view:main-view 
		    (list
		     :impl *impl-info*
		     :body (restas:render-object 
			    (find-package pkg)
			    obj)))))))

(defparameter *db-spec* '("zlodb" "lisp" "lisp" "localhost" :pooled-p t))
(defparameter *wait-on-timeout* 60)
(defparameter *wait-after-block* 20)
(defparameter *block-size* 100)

(restas:mount-submodule #:lweb-viewer (#:lweb)
  (*default-render-method* (render-method-for-package ':lweb.subview))
  (lweb::*db-spec* *db-spec*)
  (lweb:*message-class* 'message)
  (lweb:*index-limit* 50)
  (lweb:*reverse-order* t))

(defun image-path (name)
  (merge-pathnames name 
		   (merge-pathnames "img/"
				    (asdf:component-pathname (asdf:find-system '#:zlorec)))))

(restas:define-module #:zlorec-daemon
    (:use #:cl #:iter)
  (:export))

(in-package #:zlorec-daemon)

(restas:mount-submodule #:zlorec (#:zlorec))

(defvar *daemon-thread* nil)
(defparameter *daemon-thread-name* "Retrieve thread")

(defun start-daemon ()
  (unless (and *daemon-thread*
	       (bordeaux-threads:thread-alive-p *daemon-thread*))
    (setf *daemon-thread* 
	  (bordeaux-threads:make-thread #'zlorec:retrieve-loop
					:name *daemon-thread-name*))))

(defun stop-daemon ()
  (if (and *daemon-thread*
	   (bordeaux-threads:thread-alive-p *daemon-thread*))
      (iter (for thread in (bordeaux-threads:all-threads))
	    (if (equal *daemon-thread-name* (bordeaux-threads:thread-name thread))
		(bordeaux-threads:destroy-thread thread)))
      (setf *daemon-thread* nil)))

(restas:define-initialization (context)
  (start-daemon))
  
