(in-package #:zlorec)

(defprepared db-user-query 
    "
select count(*) from actual_dataset
where author = $1
"
   :single)

(defprepared db-user-date-query
    "
select count(*) from actual_dataset
where author = $1
and date >= $2
and date < $3
"
  :single)

(defprepared db-total-date-query
    "
select count(*) from message
where date >= $1
and date < $2
"
  :single)

(defprepared db-user-unit-query
    "
select count(*) from actual_dataset
where author = $1 and date_part($2, date) = $3
"
  :single)

(defprepared db-total-unit-query
    "
select count(*) from actual_dataset
where date_part($1, date) = $2
"
  :single)

(defprepared db-earliest-post
    "
select min(date) from message
"
  :single)

(defprepared db-earliest-post-user
    "
select min(date) from message
where author = $1
"
  :single)

(defprepared db-create-user-actual-dataset
    "
create temp table actual_dataset as 
select * from message 
where 
author = $1 and date >= $2
")

(defprepared db-create-total-actual-dataset
    "
create temp table actual_dataset as 
select * from message 
where date >= $1
")

(defprepared db-drop-actual-dataset
    "drop table if exists actual_dataset")

(defmacro with-user-dataset ((user age) &body body)
  `(progn
     (db-drop-actual-dataset)
     (db-create-user-actual-dataset ,user (apply #'timestamp- (now) ,age))
     (unwind-protect (progn ,@body)
       (db-drop-actual-dataset))))

(defmacro with-total-dataset ((age) &body body)
  `(progn
     (db-drop-actual-dataset)
     (db-create-total-actual-dataset (apply #'timestamp- (now) ,age))
     (unwind-protect (progn ,@body)
       (db-drop-actual-dataset))))

(defun user-post-number (user &key range)
  (if range
      (db-user-date-query user
			  (getf range :from)
			  (getf range :to))
      (db-user-query user)))

(defun total-post-number (range)
  (db-total-date-query (getf range :from)
		       (getf range :to)))

(defun unit-query-limit (&optional (range *unit-query-limit*))
  (apply #'timestamp- (now) range))

(defun user-unit-post-number (user unit value)
  (db-user-unit-query user
		      (princ-to-string unit) 
		      value))

(defun total-unit-post-number (unit value)
  (db-total-unit-query (princ-to-string unit) 
		       value))
    
(defun earliest-post (&optional user)
  (let ((date (if user
		  (db-earliest-post-user user)
		  (db-earliest-post))))
    (if (typep date 'db-null)
	(unix-to-timestamp 0)
	date)))
