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
  (with-connection *db-spec*
    (if range
	(db-user-date-query user
			    (getf range :from)
			    (getf range :to))
	(db-user-query user))))

(defparameter *date-units* `((:year  ,#'timestamp-year)
			     (:month ,#'timestamp-month)
			     (:day   ,#'timestamp-day)
			     (:hour  ,#'timestamp-hour)))

(defun user-post-number-around (&key user date unit)
  (let ((flag nil))
    (flet ((insignificant (cur-unit)
	     (prog1
		 flag
	       (if (eq unit cur-unit)
		   (setf flag t)))))
      (with-connection *db-spec* 
	(query (:select (:count :*) :from 'message
			:where (:and (:or (not user)
					  (:= :author (or user "")))
				     (:or (insignificant :year)
					   (:= (:extract :year 'date) 
					       (timestamp-year date)))
				     (:or (insignificant :month)
					   (:= (:extract :month 'date) 
					       (timestamp-month date)))
				     (:or (insignificant :day)
					   (:= (:extract :day 'date) 
					       (timestamp-day date)))
				     (:or (insignificant :hour)
					   (:= (:extract :hour 'date) 
					       (timestamp-hour date)))))
	       :single)))))
			    
(defun total-post-number (range)
  (with-connection *db-spec* 
    (db-total-date-query (getf range :from)
			 (getf range :to))))
    
