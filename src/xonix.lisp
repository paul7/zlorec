(in-package #:zlorec)

(defparameter *user-query* "http://zlo.rt.mipt.ru:7500/search?st=all&text=&topic=-1&inTitle=on&inBody=on&nick=~a&host=&site=0&pageSize=0&submitBtn=%C8%F1%EA%E0%F2%FC%21")

(defparameter *user-date-query* "http://zlo.rt.mipt.ru:7500/search?st=all&text=&topic=-1&inTitle=on&inBody=on&nick=~a&host=&dates=on&fd=~a&td=~a&site=0&pageSize=0&submitBtn=%C8%F1%EA%E0%F2%FC%21")

(defparameter *message-number-regex* "<span class=\"pagebanner\">Найдено сообщений: ([\\d,]+), показаны")

(defparameter *xonix-encoding* :windows-1251)

(defun normalize-timestamp (timestamp)
  (format nil "~2,1,0,'0@a.~2,1,0,'0@a.~a"
	  (timestamp-day timestamp)
	  (timestamp-month timestamp)
	  (timestamp-year timestamp)))

(defun user-query (user)
  (format nil *user-query* 
	  (hunchentoot:url-encode user)))

(defun user-date-query (user range)
  (format nil *user-date-query* 
	  (hunchentoot:url-encode user *xonix-encoding*)
	  (hunchentoot:url-encode (normalize-timestamp (getf range :from)))
	  (hunchentoot:url-encode (normalize-timestamp (getf range :to)))))

(defun normalize-xonix-output (response)
  (let ((numbers (nth-value 1 (scan-to-strings *message-number-regex* 
					       response))))
    (if numbers
	(nth-value 0 (parse-integer (regex-replace-all "," (elt numbers 0) "")))
	0)))

(defun user-post-number% (user &key range)
  (let ((query))
    (setf query (if range 
		    (user-date-query user range)
		    (user-query user)))
    (normalize-xonix-output (http-request query))))
					  
			    
