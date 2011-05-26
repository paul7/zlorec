(in-package #:zlorec)

(defclass message (lweb:message-mixin)
  ((id        :col-type integer 
	      :initarg :id
	      :accessor message-id
	      :reader   lweb:render-id
	      :reader   lweb:render-display-id)
   (text      :col-type text 
	      :initarg :text 
	      :accessor message-text
	      :reader   lweb:render-text)
   (header    :col-type (varchar 256)
	      :initarg :header 
	      :accessor message-header
	      :reader   lweb:render-header)
   (visible   :col-type boolean
	      :initform t 
	      :initarg :visible 
	      :accessor message-visible
	      :reader   lweb:render-visible)
   (parent-id :col-type integer
	      :initarg :parent-id 
	      :accessor message-parent-id
	      :reader   lweb:render-parent-id
	      :foreign-key (message id))
   (root-id   :col-type integer 
	      :initarg :root-id 
	      :accessor message-root-id
	      :reader   lweb:render-root-id
	      :foreign-key (message id))
   (author    :col-type (varchar 80)
	      :initarg :author
	      :accessor message-author)
   (unreg     :col-type boolean
	      :initform nil
	      :initarg :unreg
	      :accessor message-unreg)
   (date      :col-type timestamp
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

(defclass comment (message) 
  ()
  (:metaclass dao-class))

(defclass db-zlo-storage (db-storage)
  ())

(defmethod id-thread-messages ((storage db-zlo-storage) id)
  (db-messages-in-thread id))

(defmethod root-ids ((storage db-zlo-storage) &key around limit)
  (if around
      (sort (db-root-ids-around around limit) #'>)
      (db-root-ids limit)))

(defprepared-with-names db-root-ids (limit) ("
select id from 
	message
where 
	parent_id = 0
order by id desc
limit $1
" limit)
  :column)

(defprepared-with-names db-root-ids-around (id limit) ("
(select id 
from 
	message 
where 
	parent_id = 0
and
	id < $1
order by id desc
limit $2)
	union
(select id 
from 
	message
where 
	parent_id = 0
and
	id >= $1
order by id
limit $2)
" id (ceiling (/ limit 2)))
  :column)

(defprepared-with-names db-messages-in-thread (id) ("
select * 
from 
	message 
where
	root_id = (select root_id from message where id = $1)
order by id desc
" id)
  (:dao message))
