(in-package #:zlorec)

(defun get-total-activity (&key (unit :hour) (amount 24))
  (with-connection *db-spec*
    (iter 
      (for period from amount downto 1)
      (collect (total-post-number
		(list :from (timestamp- (now) period unit)
		      :to   (timestamp- (now) (1- period) unit)))))))

(defun get-user-activity (user &key (unit :month) (amount 12))
  (with-connection *db-spec*
    (iter 
      (for period from amount downto 1)
      (collect (user-post-number user 
				 :range (list :from (timestamp- (now) period unit)
					      :to   (timestamp- (now) (1- period) unit)))))))						   
  
