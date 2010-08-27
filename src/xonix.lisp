(in-package #:zlorec)

(defvar *user-query* "http://zlo.rt.mipt.ru:7500/search?st=all&text=&topic=-1&inTitle=on&inBody=on&nick=~a&host=&site=0&pageSize=0&submitBtn=%C8%F1%EA%E0%F2%FC%21")

(defvar *user-date-query* "http://zlo.rt.mipt.ru:7500/search?st=all&text=&topic=-1&inTitle=on&inBody=on&nick=~a&host=&dates=on&fd=~a&td=~a&site=0&pageSize=0&submitBtn=%C8%F1%EA%E0%F2%FC%21")

(defvar *message-number-regex* "<span class=\"pagebanner\">Найдено сообщений: ([\\d,]+), показаны")

(defvar *xonix-encoding* :windows-1251)

(defun normalize-xonix-output (response)
  (let ((numbers (nth-value 1 (scan-to-strings *message-number-regex* 
					       response))))
    (if numbers
	(nth-value 0 (parse-integer (regex-replace-all "," (elt numbers 0) "")))
	0)))

(defun user-post-number (user &key range)
  (let ((query))
    (setf query (if range 
		    (format nil *user-date-query* 
			    (hunchentoot:url-encode user *xonix-encoding*)
			    (hunchentoot:url-encode (getf range :from)) 
			    (hunchentoot:url-encode (getf range :to)))
		    (format nil *user-query* 
			    (hunchentoot:url-encode user))))
    (normalize-xonix-output (http-request query))))
					  
			    
