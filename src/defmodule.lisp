(restas:define-module #:zlorec
    (:use #:cl #:postmodern #:drakma #:cl-ppcre #:local-time #:parse-html #:iterate #:lweb #:alexandria)
  (:export #:retrieve-loop
	   #:*wait-on-timeout*
	   #:*wait-after-block*
	   #:*block-size*
	   #:message
	   #:find-last-id
	   #:max-message-id))

(in-package #:zlorec)

(defparameter *db-spec* '("zlodb" "lisp" "lisp" "localhost" :pooled-p t))
(defparameter *memo-storage* (pm:make-db-storage *db-spec*))

(defparameter *unit-query-limit* '(3 :month))

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

(defparameter *wait-on-timeout* 60)
(defparameter *wait-after-block* 20)
(defparameter *block-size* 100)

(restas:mount-submodule #:lweb-viewer (#:lweb)
  (*default-render-method* (render-method-for-package ':lweb.subview))
  (lweb::*db-spec* *db-spec*)
  (lweb:*message-class* 'message)
  (lweb:*index-limit* 20)
  (lweb:*reverse-order* t))

(defun image-path (name)
  (merge-pathnames name 
		   (merge-pathnames "img/"
				    (asdf:component-pathname (asdf:find-system '#:zlorec)))))

(restas:define-module #:zlorec-daemon
    (:use #:cl #:iter #:alexandria)
  (:export))

(in-package #:zlorec-daemon)

(restas:mount-submodule #:zlorec (#:zlorec))

(defvar *daemon-thread-name* "Retrieve thread")

(defvar *watchdog-thread-name* "Watchdog thread")
(defparameter *watchdog-timeout* 60)

(defmacro define-thread (symbol function)
  (let ((start-fun (symbolicate 'start- symbol))
	(stop-fun  (symbolicate 'stop- symbol))
	(name      (symbolicate '* symbol '-thread-name*))
	(thread    (symbolicate '* symbol '-thread*)))
  `(progn 
     (defvar ,thread nil)
     (defvar ,name nil)
     (defun ,start-fun ()
       (unless (and ,thread
		    (bordeaux-threads:thread-alive-p ,thread))
	 (setf ,thread
	       (bordeaux-threads:make-thread ,function
					     :name ,name))))
     (defun ,stop-fun ()
       (when (and ,thread
		  (bordeaux-threads:thread-alive-p ,thread))
	 (bordeaux-threads:destroy-thread ,thread))
       (setf ,thread nil)))))

(defun watchdog ()
  (iter
    (for max-id next (zlorec:max-message-id))
    (for prev-max-id previous max-id initially 0)
    (for last-id next (zlorec:find-last-id))
    (when (and (< max-id last-id)
	       (= max-id prev-max-id)) ; we're stuck
      (format t "max-id: ~a last-id: ~a; restarting retrieve~%" 
	      max-id last-id)
      (stop-daemon)
      (start-daemon))
    (sleep *watchdog-timeout*)))

(define-thread daemon #'zlorec:retrieve-loop)
(define-thread watchdog #'watchdog)

(restas:define-initialization (context)
  (start-daemon)
  (start-watchdog))
