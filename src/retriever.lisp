(in-package #:zlorec)

(defclass message ()
  ((id        :col-type integer 
	      :accessor message-id)
   (text      :col-type text 
	      :initarg :text 
	      :accessor message-text)
   (header    :col-type (varchar 256)
	      :initarg :header 
	      :accessor message-header)
   (visible   :col-type boolean
	      :initform t 
	      :initarg :visible 
	      :accessor message-visible)
   (parent-id :col-type integer
	      :initform 0
	      :initarg :parent-id 
	      :accessor message-parent-id
	      :foreign-key (message id))
   (root-id   :col-type integer 
	      :initform 0
	      :initarg :root-id 
	      :accessor message-root-id
	      :foreign-key (message id))
   (author    :col-type (varchar 80)
	      :initform 1
	      :initarg :author
	      :accessor message-author)
   (children~ :initform nil
	      :initarg :children
	      :accessor message-children~)
   (thread~   :initform nil
	      :initarg :thread
	      :accessor message-thread~))
  (:keys id)
  (:metaclass dao-class))

(defparameter *post-query* "http://zlo.rt.mipt.ru/?read=~a")

(defparameter *zlo-encoding* :windows-1251)

(defparameter *div-regex* (create-scanner "^d(\\d+)$"))
(defparameter *span-regex* (create-scanner "^m(\\d+)$"))

(defun post-query (id)
  (format nil *post-query* id))

(defun match-element-id (regex id)
  (let ((captured (nth-value 1 (scan-to-strings regex id))))
    (if captured
	(parse-integer (elt captured 0)))))

(defun retreive-post (id)
  (let ((html (http-request (post-query id) 
			    :external-format-in *zlo-encoding*))
	(parent nil))
    (flet ((process-div (div)
	     (unless parent
	       (destructuring-bind ((tag &rest plist) &rest body) div
		 (declare (ignore tag))
		 (let ((parent-id (match-element-id *div-regex* 
						    (getf plist :id))))
		   (if parent-id
		       (loop 
			  :for elt :in body 
			  :until parent
			  :do
			  (destructuring-bind ((tag &rest plist) &rest body)
			      elt
			    (declare (ignore body))
			    (if (and (eq tag :span)
				     (equal (match-element-id *span-regex* 
							      (getf plist :id)) 
					    id))
				(setf parent parent-id))))))))))
      (parse-html html 
		  :callback-only t
		  :callbacks `((:div . ,#'process-div)))
      (print parent))))

     
    
  
