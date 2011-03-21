(in-package #:zlorec)

(defparameter *post-query* "http://zlo.rt.mipt.ru/?read=~a")
(defparameter *index-query* "http://zlo.rt.mipt.ru/")

(defparameter *zlo-encoding* :windows-1251)

(defparameter *div-regex* (create-scanner "^d(\\d+)$"))
(defparameter *span-regex* (create-scanner "^m(\\d+)$"))
(defparameter *time-regex* (create-scanner "(\\d+)/(\\d+)/(\\d+) (\\d+):(\\d+)"))
(defparameter *last-id-regex* (create-scanner "current_nm=(\\d+);"))

(defun post-query (id)
  (format nil *post-query* id))

(defun match-number (regex id)
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
  (set-local-time-cl-postgres-readers)
  
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

(defmacro on-text (element (&key text) &body body)
  (with-gensyms (gelt)
    `(let ((,gelt ,element))
       (if (atom ,gelt)
	   (let ,(if text
		     `((,text ,gelt)))
	     ,@body)))))

(defmacro on-tag (element 
		  (&key attrs tag inner)
		  &body body)
  (let ((attrs (group attrs 2)))
    (with-gensyms (gtag ginner gplist gelt)
      `(let ((,gelt ,element))
	 (unless (atom ,gelt)
	   (destructuring-bind (,gtag &rest ,ginner) ,gelt
	     (declare (ignorable ,ginner))
	     (destructuring-bind (,gtag &rest ,gplist) (if (consp ,gtag)
							   ,gtag
							   (list ,gtag))
	       (declare (ignorable ,gtag ,gplist))
	       (let ,(nconc (if tag
				`((,tag ,gtag)))
			    (if inner
				`((,inner ,ginner)))
			    (mapcan #'(lambda (attr)
					`((,(cadr attr) (getf ,gplist ,(car attr)))))
				    attrs))
		 ,@body))))))))

(defun parse-post (id html)
  (let ((parent nil)
	(header nil)
	(text nil)
	(author nil)
	(root nil)
	(date nil)
	(unreg nil))
    (flet ((process-div (div)
	     (on-tag div (:attrs (:id div-id :align div-align :class div-class) :inner body)
	       (let ((parent-id (match-number *div-regex* div-id)))
		  (cond 
		    ((and (not parent) parent-id)
		     (iter 
		       (for elt in body)
		       (until parent)
		       (on-tag elt (:tag tag :attrs (:id span-id))
			 (when (and (eq tag :span)
				    (eql (match-number *span-regex* span-id)
					 id))
			   (setf parent parent-id)))))
		    ((and (not root)
			  (equalp div-class "w"))
		     (iter 
		       (for elt in body)
		       (until root)
		       (on-tag elt (:tag tag :attrs (:id span-id))
			 (let ((span-id (match-number *span-regex* span-id)))
			   (if (and (eq tag :span)
				    span-id)
			       (setf root span-id))))))
		    ((and (not header) 
			  (not author)
			  (equalp div-align "center"))
		     (iter
		       (for elt in body)
		       (until (and header author))
		       (on-tag elt (:tag tag :attrs (:class elt-class) :inner elt-body) 
			 (cond  
			   ((eq tag :big)
			    (setf header (list (car elt-body))))
			   ((eq tag :b)
			    (setf author (car elt-body))
			    (setf unreg t))
			   ((and (eq tag :a)
				 (equalp elt-class "nn"))
			    (setf author (car elt-body))
			    (setf unreg nil))))))
		    ((and (not text) (equalp div-class "body"))
		     (setf text body))))))
	   (process-span (span)
	     (on-tag span (:attrs (:id span-id) :inner body)
	       (when (eql (match-number *span-regex* span-id)
			  id)
		 (iter 
		   (for elt in body)
		   (until date)
		   (on-text elt (:text date-string)
		     (let ((span-date (match-date date-string)))
		       (if span-date
			   (setf date span-date)))))))))
      (parse-html html 
		  :callback-only t
		  :callbacks `((:div . ,#'process-div)
			       (:span . ,#'process-span)))
      (when header
	(if (and date author root (atom author))
	    (make-instance 'message 
			   :id id
			   :author author
			   :unreg unreg
			   :root-id (or root 0)
			   :parent-id (or parent 0)
			   :date date
			   :header (html-gen header)
			   :text (html-gen text))
	    (make-instance 'bad-message
			   :id id
			   :html html))))))

(defun retrieve-post (id)
  (let ((html (http-request (post-query id) 
			    :external-format-in *zlo-encoding*)))
    (parse-post id html)))

(defun find-last-id ()
  (let* ((html (http-request *index-query* 
			     :external-format-in *zlo-encoding*))
	 (max-id (match-number *last-id-regex* html)))
    (or max-id 0)))

(defun install ()
  (with-connection *db-spec*
    (execute (dao-table-definition 'bad-message))
    (execute (dao-table-definition 'message))
    (execute (:create-index 'author-index :on :message 
			    :fields :author))))

(defun uninstall ()
  (with-connection *db-spec*
    (if (yes-or-no-p "This operation will purge all data. Proceed?")
	(execute (:drop-table 'message))
	(execute (:drop-table 'bad-message)))
    (values)))

(defun bulk-retrieve (from to)
  (with-connection *db-spec*
    (let ((last-ok nil))
      (iter
	(for i from from to to)
	(let ((msg (retrieve-post i)))
	  (when msg
	    (insert-dao msg)
	    (setf last-ok i))))
      last-ok)))

(defun max-message-id (&key (classes '(message bad-message)))
  (with-connection *db-spec*
    (iter 
      (for class in classes)
      (maximize (query (:select (:coalesce (:max 'id) 0) :from class) :single)))))

(defun message-count (&key (classes '(message bad-message)))
  (with-connection *db-spec*
    (iter 
      (for class in classes)
      (sum (query (:select (:count 'id) :from class) :single)))))

(defun retrieve-loop (&key 
		      (amount *block-size*)
		      (wait-on-timeout *wait-on-timeout*) 
		      (wait-after-block *wait-after-block*))
  (with-connection *db-spec*
    (iter
      (let* ((from (1+ (max-message-id)))
	     (to (min (find-last-id)
		      (1- (+ amount from)))))
	(incf *retrieve-run-time*)
	(handler-case (progn
			(when (>= to from)
			  (format t "starting retrieve: from ~a to ~a~%" 
				  from to)
			  (bulk-retrieve from to))
			(sleep wait-after-block))
	  (error (er)
	    (format t "Problem after message ~a:~%~a~%"
		    (max-message-id)
		    er)
	    (sleep wait-on-timeout)))))))

(defun fix-header-problem ()
  (with-connection *db-spec* 
    (let ((ids (query "select id from message where header like '<A%'" :column)))
      (iter (for id in ids)
	    (print id)
	    (query (:delete-from 'message :where (:= 'id id)))
	    (bulk-retrieve id id)))))
		   
