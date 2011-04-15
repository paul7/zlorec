(in-package #:zlorec)

(defclass message (lweb:message-mixin)
  ((id         :col-type serial
	       :initarg :id
	       :accessor message-id
	       :reader   lweb:render-id)
   (archive-id :col-type integer
	       :initarg :archive-id
	       :accessor message-archive-id)
   (text       :col-type text 
	       :initarg :text 
	       :accessor message-text
	       :reader   lweb:render-text)
   (header     :col-type (varchar 256)
	       :initarg :header 
	       :accessor message-header
	       :reader   lweb:render-header)
   (visible    :col-type boolean
	       :initform t 
	       :initarg :visible 
	       :accessor message-visible
	       :reader   lweb:render-visible)
   (parent-id  :col-type integer
	       :initarg :parent-id 
	       :accessor message-parent-id
	       :reader   lweb:render-parent-id
	       :foreign-key (message id))
   (root-id    :col-type integer 
	       :initarg :root-id 
	       :accessor message-root-id
	       :reader   lweb:render-root-id
	       :foreign-key (message id))
   (author     :col-type (varchar 80)
	       :initarg :author
	       :accessor message-author)
   (unreg      :col-type boolean
	       :initform nil
	       :initarg :unreg
	       :accessor message-unreg)
   (date       :col-type timestamp
	       :initarg :date
	       :accessor message-date))
  (:keys id)
  (:metaclass dao-class))

(defmethod lweb:render-author ((message message))
  (list :id 1 :nick (message-author message)))

(defclass bad-message ()
  ((id        :col-type integer
	      :initarg :id
	      :accessor bad-message-id)
   (html      :col-type text
	      :initarg :html
	      :accessor bad-message-html))
  (:keys id)
  (:metaclass dao-class))

(defclass db-comment-storage (db-storage)
  ())

(defmethod id-thread-messages ((storage db-comment-storage) id)
  (make-instances 'message 
		  (db-messages-in-thread/with-comments id)))

(defmethod root-ids ((storage db-comment-storage) &key around limit)
  (if around
      (db-root-ids-around around limit)
      (db-root-ids limit)))
