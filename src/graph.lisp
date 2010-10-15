(in-package #:zlorec)

(setf *default-render-method*
      (render-method-for-package ':zlorec.view))

(restas:define-route main ("")
  (restas:redirect 'graph-form))

(restas:define-route graph-form ("graph-form")
  (list :graphurl (restas:genurl 'graph)))

(restas:define-route graph ("graph"
			    :method :post
			    :requirement #'(lambda () (hunchentoot:post-parameter "send")))
  (list :graph (restas:genurl 'user-activity 
			      :user   (hunchentoot:post-parameter "user")
			      :unit   (hunchentoot:post-parameter "unit")
			      :amount (hunchentoot:post-parameter "amount"))
	:return (restas:genurl 'graph-form)))

(defun validate-unit (unit)
  (cond 
    ((equalp unit "day")   :day)
    ((equalp unit "month") :month)
    ((equalp unit "year")  :year)))

(defparameter *graph-height* 400)
(defparameter *graph-width* 400)

(defun render-svg-bar-graph (values &key (title "") subscripts)
  (declare (ignore title subscripts))
  (let* ((amount (length values))
	 (max-value (+ 0.1 (apply #'max values)))
	 (bar-width (floor (/ *graph-width* amount)))
	 (scaled (iter 
		   (for offset from 0)
		   (for val in values)
		   (collect
		       (list :y      (floor (* (* val 0.9) 
					       (/ *graph-height* max-value)))
			     :x      bar-width
			     :offset (* offset bar-width)
			     :bottom *graph-height*))
			 values)))
    (list :values scaled
	  :width  (* amount bar-width)
	  :height *graph-height*)))

(pm:define-memoized-route user-activity ("useract.svg/:user/:unit/:amount"
					 :parse-vars (list :amount #'parse-integer
							   :unit   #'validate-unit
							   :user   #'hunchentoot:url-decode)
					 :render-method #'zlorec.view:svg-bar-graph
					 :content-type "image/svg+xml")
  (if (and (plusp (length user))
	   (plusp amount))
      (values 
       (render-svg-bar-graph (get-user-activity user :unit unit :amount amount))
       '(1 :hour))
      (list :error "bad parameters")))
		      
(restas:define-route retrieved ("zlorec")
  (list :lastid  (max-message-id)
	:lastbad (max-message-id :classes '(bad-message))
	:msgnum  (message-count)
	:badnum  (message-count :classes '(bad-message))))
