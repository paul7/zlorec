(in-package #:zlorec)

(defclass message ()
  ((id        :col-type integer 
	      :accessor message-id)
   (text      :col-type text 
	      :initform "hello world" 
	      :initarg :text 
	      :accessor message-text)
   (header    :col-type (varchar 256)
	      :initform "hello" 
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

(defun retreive-post (id)
  (declare (ignore id)))
  
  
