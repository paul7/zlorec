(in-package #:zlorec)

(defclass message ()
  ((id        :col-type integer 
	      :initarg :id
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
	      :initarg :parent-id 
	      :accessor message-parent-id
	      :foreign-key (message id))
   (root-id   :col-type integer 
	      :initarg :root-id 
	      :accessor message-root-id
	      :foreign-key (message id))
   (author    :col-type (varchar 80)
	      :initarg :author
	      :accessor message-author)
   (unreg     :col-type boolean
	      :initform nil
	      :initarg :unreg
	      :accessor message-unreg)
   (date      :col-type date
	      :initarg :date
	      :accessor message-date)
   (children~ :initform nil
	      :initarg :children~
	      :accessor message-children~)
   (thread~   :initform nil
	      :initarg :thread~
	      :accessor message-thread~))
  (:keys id)
  (:metaclass dao-class))

(defparameter *post-query* "http://zlo.rt.mipt.ru/?read=~a")

(defparameter *zlo-encoding* :windows-1251)

(defparameter *div-regex* (create-scanner "^d(\\d+)$"))
(defparameter *span-regex* (create-scanner "^m(\\d+)$"))
(defparameter *time-regex* (create-scanner "(\\d+)/(\\d+)/(\\d+) (\\d+):(\\d+)"))

(defun post-query (id)
  (format nil *post-query* id))

(defun match-element-id (regex id)
  (let ((captured (nth-value 1 (scan-to-strings regex id))))
    (if captured
	(parse-integer (elt captured 0)))))

(defun match-date (string)
  (let ((captured (nth-value 1 (scan-to-strings *time-regex* string))))
    (if captured 
	(destructuring-bind (day month year hour minute) 
	    (mapcar #'parse-integer (coerce captured 'list))
	  (encode-timestamp 0 0 minute hour day month year)))))

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

(defmacro destruct-tag (((&rest attrs) &key tag body) elt &key on-tag on-text)
  (let ((gtag (gensym))
	(gbody (gensym))
	(gplist (gensym))
	(attrs (group attrs 2)))
    `(if (atom ,elt)
	 ,(if on-text
	      `(let ((,gtag nil)
		     (,gbody ,elt)
		     (,gplist nil))
		 (declare (ignorable ,gbody ,gtag ,gplist))
		 (let ,(nconc (if body
				  `((,body ,gbody))))
		   ,@on-text)))
	 ,(if on-tag
	      `(destructuring-bind (,gtag &rest ,gbody) ,elt
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
		     ,@on-tag)))))))

(defun parse-post (id html)
  (let ((parent nil)
	(header nil)
	(text nil)
	(author nil)
	(root nil)
	(date nil)
	(unreg nil))
    (flet ((process-div (div)
	     (destruct-tag ((:id div-id :align div-align :class div-class) :body body) div
	       :on-tag
	       ((let ((parent-id (match-element-id *div-regex* div-id)))
		  (cond 
		    ((and (not parent) parent-id)
		     (loop 
			:for elt :in body 
			:until parent
			:do
			(destruct-tag ((:id span-id) :tag tag) elt
			  :on-tag
			  ((when (and (eq tag :span)
				    (eql (match-element-id *span-regex* span-id)
					 id))
			     (setf parent parent-id))))))
		    ((and (not root)
			  (equalp div-class "w"))
		     (loop 
			:for elt :in body
			:until root
			:do
			(destruct-tag ((:id span-id) :tag tag) elt
			  :on-tag
			  ((let ((span-id (match-element-id *span-regex* span-id)))
			    (if (and (eq tag :span)
				     span-id)
				(setf root span-id)))))))
		    ((and (not header) 
			  (not author)
			  (equalp div-align "center"))
		     (loop 
			:for elt :in body
			:until (and header author)
			:do
			(destruct-tag ((:class elt-class) 
				       :tag tag :body elt-body) elt
			  :on-tag
			  ((cond  
			    ((eq tag :big)
			     (setf header (car elt-body)))
			    ((eq tag :b)
			     (setf author (car elt-body))
			     (setf unreg t))
			    ((and (eq tag :a)
				  (equalp elt-class "nn"))
			     (setf author (car elt-body))
			     (setf unreg nil)))))))
		    ((and (not text) (equalp div-class "body"))
		     (setf text body)))))))
	   (process-span (span)
	     (destruct-tag ((:id span-id) :body body) span
	       :on-tag
	       ((when (eql (match-element-id *span-regex* span-id)
			   id)
		  (loop 
		     :for elt :in body
		     :until date
		     :do
		     (destruct-tag (() :body date-string) elt
		       :on-text
		       ((let ((span-date (match-date date-string)))
			  (if span-date
			      (setf date span-date)))))))))))
      (parse-html html 
		  :callback-only t
		  :callbacks `((:div . ,#'process-div)
			       (:span . ,#'process-span)))
      (if header
	  (make-instance 'message 
			 :id id
			 :author author
			 :unreg unreg
			 :root-id (or root 0)
			 :parent-id (or parent 0)
			 :date (timestamp-to-unix date)
			 :header (html-gen header)
			 :text (html-gen text))))))

(defun retrieve-post (id)
  (let ((html (http-request (post-query id) 
			    :external-format-in *zlo-encoding*)))
    (parse-post id html)))

(defparameter *db-spec* '("zlodb" "lisp" "lisp" "localhost"))

(defun install ()
  (with-connection *db-spec*
    (execute (dao-table-definition 'message))
    (execute (:create-index 'author-index :on :message 
			    :fields :author))))

(defun uninstall ()
  (with-connection *db-spec*
    (if (yes-or-no-p "This operation will purge all data. Proceed?")
	(execute (:drop-table 'message)))
    (values)))

(defun bulk-retrieve (from to)
  (with-connection *db-spec*
    (let ((last-ok nil))
      (loop
	 :for i :from from :to to
	 :do
	 (let ((msg (retrieve-post i)))
	   (when msg
	     (insert-dao msg)
	     (setf last-ok i))))
      last-ok)))
