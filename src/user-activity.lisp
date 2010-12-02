(in-package #:zlorec)

(defun timestamp-subscript-part (timestamp unit)
  (ecase unit
    (:year  (format nil "'~2,1,0,'0@a" (mod (timestamp-year timestamp) 100)))
    (:month (timestamp-month timestamp))
    (:day   (timestamp-day timestamp))
    (:hour  (timestamp-hour timestamp))
    (:min   (timestamp-minute timestamp))))

(defparameter *start-year* 
  (with-connection *db-spec* 
    (timestamp-year (earliest-post))))

(defun unit-range (unit)
  (values
   (ecase unit
     (:year  *start-year*)
     (:month 1)
     (:day   1)
     (:hour  0))
   (ecase unit
     (:year  (timestamp-year (now)))
     (:month 12)
     (:day   31)
     (:hour  23))))

(defmacro define-activity (name (&rest args) &body body)
  `(defun ,name (&key (now (now)) unit amount ,@args)
     (with-connection *db-spec*
       (iter 
	 (for period from amount downto 1)
	 (for date initially (timestamp- now (1- amount) unit) then (timestamp+ date 1 unit))
	 (collect (progn ,@body)
	   into numbers)
	 (collect (timestamp-subscript-part date unit)
	   into labels)
	 (finally (return (values numbers labels)))))))

(define-activity get-total-activity ()
  (total-post-number
   (list :from (timestamp- date 1 unit)
	 :to   date)))

(define-activity get-user-activity (user)
  (user-post-number user :range (list :from (timestamp- date 1 unit)
				      :to   date)))
	  
(defun timestamp-next-unit (unit)
  (ecase unit
    (:year  :month)
    (:month :day)
    (:day   :hour)
    (:hour  :min)))

(defun get-total-activity-calendar (&key (now (now)) (unit :hour) (amount 24))
  (get-total-activity :now (timestamp-maximize-part now (timestamp-next-unit unit))
		      :unit unit
		      :amount amount))

(defun get-user-activity-calendar (&key (now (now)) (unit :month) (amount 12) user)
  (get-user-activity :user user
		     :now (timestamp-maximize-part now (timestamp-next-unit unit))
		     :unit unit
		     :amount amount))


(defun get-user-unit-activity (user unit)
  (multiple-value-bind (start end) (unit-range unit)
    (with-connection *db-spec* 
      (iter 
	(for value from start to end)
	(collect (user-unit-post-number user unit value) into numbers)
	(collect value into labels)
	(finally (return (values numbers labels)))))))

(defun get-total-unit-activity (unit)
  (multiple-value-bind (start end) (unit-range unit)
    (with-connection *db-spec* 
      (iter 
	(for value from start to end)
	(collect (total-unit-post-number unit value) into numbers)
	(collect value into labels)
	(finally (return (values numbers labels)))))))
