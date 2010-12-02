(in-package #:zlorec)

(defprepared db-user-query 
    (:select (:count :*) :from 'message
	     :where (:= :author '$1)) 
  :single)

(defprepared db-user-date-query
    (:select (:count :*) :from 'message
	     :where (:and
		     (:= :author '$1)
		     (:>= :date '$2)
		     (:< :date '$3)))
  :single)

(defprepared db-total-date-query
    (:select (:count :*) :from 'message
	     :where (:and
		     (:>= :date '$1)
		     (:< :date '$2)))
  :single)

(defprepared db-user-unit-query
    (:select (:count :*) :from 'message 
	     :where (:and
		     (:>= :date '$4)
		     (:= :author '$1)
		     (:= (:date-part '$2 :date) '$3)))
  :single)

(defprepared db-total-unit-query
    (:select (:count :*) :from 'message 
	     :where (:and
		     (:>= :date '$3)
		     (:= (:date-part '$1 :date) '$2)))
  :single)

(defprepared db-earliest-post
    (:select (:min 'date) :from 'message)
  :single)

(defprepared db-earliest-post-user
    (:select (:min 'date) :from 'message
	     :where (:= :author '$1))
  :single)

(defun user-post-number (user &key range)
  (if range
      (db-user-date-query user
			  (getf range :from)
			  (getf range :to))
      (db-user-query user)))

(defun total-post-number (range)
  (db-total-date-query (getf range :from)
		       (getf range :to)))

(defparameter *unit-query-limit* '(3 :month))

(defun unit-query-limit (&optional (range *unit-query-limit*))
  (apply #'timestamp- (now) range))

(defun user-unit-post-number (user unit value 
			      &key (range *unit-query-limit*))
  (db-user-unit-query user 
		      (princ-to-string unit) 
		      value 
		      (unit-query-limit range)))

(defun total-unit-post-number (unit value
			       &key (range *unit-query-limit*))
  (db-total-unit-query (princ-to-string unit) 
		       value 
		       (unit-query-limit range)))
    
(defun earliest-post (&optional user)
  (let ((date (if user
		  (db-earliest-post-user user)
		  (db-earliest-post))))
    (if (typep date 'db-null)
	(unix-to-timestamp 0)
	date)))
