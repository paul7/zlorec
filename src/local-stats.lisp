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

(defun user-post-number (user &key range)
  (if range
      (db-user-date-query user
			  (getf range :from)
			  (getf range :to))
      (db-user-query user)))

(defun total-post-number (range)
  (db-total-date-query (getf range :from)
		       (getf range :to)))
    
