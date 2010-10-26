(in-package #:zlorec)

(defun get-total-activity (&key (unit :hour) (amount 24))
  (get-user-activity-calendar :unit unit 
			      :amount amount))

(defun get-user-activity (user &key (unit :month) (amount 12))
  (with-connection *db-spec*
    (iter 
      (for period from amount downto 1)
      (collect (user-post-number user 
				 :range (list :from (timestamp- (now) period unit)
					      :to   (timestamp- (now) (1- period) unit)))))))						   
(defun get-user-activity-calendar (&key user (now (now)) (unit :month) (amount 12))
  (with-connection *db-spec* 
    (iter
      (for i from (1- amount) downto 0)
      (collect (user-post-number-around :user user 
					:date (timestamp- now i unit) 
					:unit unit)))))

