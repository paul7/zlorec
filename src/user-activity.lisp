(in-package #:zlorec)

(defun normalize-timestamp (timestamp)
  (format nil "~2,1,0,'0@a.~2,1,0,'0@a.~a"
	  (timestamp-day timestamp)
	  (timestamp-month timestamp)
	  (timestamp-year timestamp)))
  
(defun get-user-activity (user &key (unit :month) (amount 12))
  (loop :for period :from amount :downto 1 
     :collect (user-post-number user 
			       :range (list :from (normalize-timestamp 
						   (timestamp- (now) period unit))
					    :to (normalize-timestamp 
						 (timestamp- (now) (1- period) unit))))))
						   
  
