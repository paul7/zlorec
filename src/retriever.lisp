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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun group (source n)
    (if (zerop n) (error "zero length"))
    (labels ((rec (source acc)
	       (let ((rest (nthcdr n source)))
		 (if (consp rest)
		     (rec rest (cons
				(subseq source 0 n)
				acc))
		     (nreverse
		      (cons source acc))))))
      (if source (rec source nil) nil))))

(defmacro destruct-tag (((&rest attrs) &key tag body) elt &body code)
  (let ((gtag (gensym))
	(gbody (gensym))
	(gplist (gensym))
	(attrs (group attrs 2)))
    `(if (consp ,elt)
	 (destructuring-bind (,gtag &rest ,gbody) ,elt
	   (declare (ignorable ,gbody))
	   (destructuring-bind (,gtag &rest ,gplist) (if (consp ,gtag)
							 ,gtag
							 (list ,gtag))
	     (declare (ignorable ,gtag ,gplist))
	     (let ,(nconc (if tag
			      `((,tag ,gtag)))
			  (if body
			      `((,body ,gbody)))
			  (mapcan #'(lambda (attr)
				      `((,(cadr attr) (getf ,gplist ,(car attr)))))
				  attrs))
	       ,@code))))))

(defun html-open-tag (tag)
  (let ((tag (if (consp tag)
		 tag
		 (list tag))))
    (with-output-to-string (s)
      (format s "<~a" (car tag))
      (mapc #'(lambda (param) 
		(format s " ~a=\"~a\"" (car param) (cadr param)))
	    (group (cdr tag) 2))
      (princ ">" s))))

(defun html-close-tag (tag)
  (let ((tag (if (consp tag)
		 tag
		 (list tag))))
    (format nil "</~a>" (car tag))))

(defun html-element (elt)
  (cond
    ((null elt)
     "")
    ((stringp elt)
     elt)
    ((keywordp elt)
     (format nil "<~a />" elt))
    ((consp elt)
     (let ((tag-info (car elt))
	   (body (cdr elt)))
       (concatenate 'string 
		    (html-open-tag tag-info)
		    (html-gen body)
		    (html-close-tag tag-info))))
    (t (error "bad lhtml"))))

(defun html-gen (lhtml)
  (if (atom lhtml)
      (html-element lhtml)
      (apply #'concatenate 'string (mapcar #'html-element lhtml))))

(defun retreive-post (id)
  (let ((html (http-request (post-query id) 
			    :external-format-in *zlo-encoding*))
	(parent nil)
	(header nil)
	(text nil)
	(author nil))
    (flet ((process-div (div)
	     (destruct-tag ((:id div-id :align div-align :class div-class) :body body) div
	       (let ((parent-id (match-element-id *div-regex* div-id)))
		 (cond 
		   ((and (not parent) parent-id)
		    (loop 
		       :for elt :in body 
		       :until parent
		       :do
		       (destruct-tag ((:id span-id) :tag tag) elt
			 (if (and (eq tag :span)
				  (eql (match-element-id *span-regex* span-id)
				       id))
			     (setf parent parent-id)))))
		   ((and (not header) 
			 (not author) 
			 (equalp div-align "center"))
		    (loop 
		       :for elt :in body
		       :until (and header author)
		       :do
		       (destruct-tag ((:class elt-class) 
				      :tag tag :body elt-body) elt
			 (cond  
			   ((eq tag :big)
			    (setf header (car elt-body)))
			   ((or (eq tag :b)
				(and (eq tag :a)
				     (equalp elt-class "nn")))
			    (setf author (car elt-body)))))))
		   ((and (not text) (equalp div-class "body"))
		    (setf text body)))))))
      (parse-html html 
		  :callback-only t
		  :callbacks `((:div . ,#'process-div)))
      (print (or parent 0))
      (print (html-gen header))
      (print (html-gen (or text "")))
      (print author)
      (values))))

     
    
  
