(in-package #:zlorec)

(defun get-total-activity (&key (now (now)) (unit :hour) (amount 24))
  (with-connection *db-spec*
    (iter 
      (for period from amount downto 1)
      (collect (total-post-number
		(list :from (timestamp- now period unit)
		      :to   (timestamp- now (1- period) unit)))))))

(defun get-user-activity (user &key (now (now)) (unit :month) (amount 12))
  (with-connection *db-spec*
    (iter 
      (for period from amount downto 1)
      (collect (user-post-number user 
				 :range (list :from (timestamp- now period unit)
					      :to   (timestamp- now (1- period) unit)))))))

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

(defun get-user-activity-calendar (user &key (now (now)) (unit :month) (amount 12))
  (get-user-activity user
		     :now (timestamp-maximize-part now (timestamp-next-unit unit))
		     :unit unit
		     :amount amount))
