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
  (let ((graph (restas:genurl 'user-activity 
			      :user     (hunchentoot:post-parameter "user")
			      :unit     (hunchentoot:post-parameter "unit")
			      :amount   (hunchentoot:post-parameter "amount")
			      :typefunc (hunchentoot:post-parameter "type"))))
    (list :graph graph
	  :return (restas:genurl 'graph-form))))

(defun validate-unit (unit)
  (cond 
    ((equalp unit "hour")  :hour)
    ((equalp unit "day")   :day)
    ((equalp unit "month") :month)
    ((equalp unit "year")  :year)))

(defun validate-type-user (type)
  (cond 
    ((equalp type "cal") #'get-user-activity-calendar)
    ((equalp type "nat") #'get-user-activity)))

(defun validate-type-total (type)
  (cond 
    ((equalp type "cal") #'get-total-activity-calendar)
    ((equalp type "nat") #'get-total-activity)))

(defparameter *graph-height* 500)
(defparameter *graph-width* 500)
(defparameter *graph-sections* 5)
(defparameter *graph-border-part* 1/10)
(defparameter *graph-max-subscripts* 12)

(defun scale-unit (max-value)
  (let* ((max-value (if (plusp max-value)
			max-value 
			1))
	 (ten-power (max 1 (expt 10 
				(floor (log (/ max-value *graph-sections*) 10))))))
    (* ten-power 
       (ceiling (/ max-value *graph-sections* ten-power)))))

(defun render-svg-bar-graph (values &key (title "") subscripts)
  (let* ((amount (length values))
	 (max-value (apply #'max values))
	 (border-width (* *graph-border-part* *graph-width*))
	 (border-height (* *graph-border-part* *graph-height*))
	 (width (- *graph-width* (* 2 border-width)))
	 (height (- *graph-height* (* 2 border-height)))
	 (bar-width (floor (/ width amount)))
	 (scale-unit (scale-unit max-value))
	 (scale-max (* *graph-sections* scale-unit))
	 (scale (/ height scale-max))
	 (subscripts (append subscripts (make-list (max 0 (- (length values)
							     (length subscripts)))
						   :initial-element "")))
	 (sub-mod (ceiling (/ (length subscripts) *graph-max-subscripts*)))
	 (scaled (iter 
		   (for offset from 0)
		   (for val in values)
		   (for sub in subscripts)
		   (collect
		       (list :y         (floor (* val scale))
			     :x         bar-width
			     :offset    (+ border-width (* offset bar-width))
			     :bottom    (+ border-height height)
			     :subscript (if (zerop (mod offset sub-mod))
					    sub
					    "")))
			 values))
	 (marks (iter 
		  (for mark from 1 below *graph-sections*)
		  (collect 
		      (let ((mark (* scale-unit mark)))
			(list :mark mark
			      :y    (- *graph-height* border-height (* scale mark))))))))
    (list :values   scaled
	  :marks    marks
	  :bwidth   border-width
	  :bheight  border-height
	  :width    (+ (* 2 border-width)  (* amount bar-width))
	  :height   *graph-height*
	  :title    title)))

(defun good-amount-p (unit amount)
  (and (plusp amount)
       (<= amount (case unit
		    (:hour     24)
		    (:day      31)
		    (:month    12)
		    (:year     12)
		    (otherwise 0)))))

(defun expiration-time (unit)
  (ecase unit
    (:year   '(1  :month))
    (:month  '(10 :day))
    (:day    '(1  :hour))
    (:hour   '(1  :hour))))

(pm:define-memoized-route user-activity ("useract.svg/:user/:unit/:amount/:typefunc"
					 :parse-vars (list :amount   #'parse-integer
							   :unit     #'validate-unit
							   :user     #'hunchentoot:url-decode
							   :typefunc #'validate-type-user)
					 :render-method #'zlorec.view:svg-bar-graph
					 :content-type "image/svg+xml")
  (cond
    ((not (plusp (length user)))
     (list :error "bad username"))
    ((not (good-amount-p unit amount))
     (list :error "bad period"))
    (t (multiple-value-bind (values subscripts) (funcall typefunc 
							 :user user 
							 :unit unit 
							 :amount amount)
	 (values 
	  (render-svg-bar-graph values
				:title      (format nil "~a (~a ~(~a~:p~))" user amount unit)
				:subscripts subscripts)
	  (expiration-time unit))))))

(pm:define-memoized-route board-activity ("pulse.svg/:typefunc"
					  :parse-vars (list :typefunc #'validate-type-total)
					  :render-method #'zlorec.view:svg-bar-graph
					  :content-type  "image/svg+xml")
  (multiple-value-bind (values subscripts) (funcall typefunc :unit :hour :amount 24)
    (values
     (render-svg-bar-graph values
			   :title      "Board activity (24 hours)"
			   :subscripts subscripts)
     (expiration-time :hour))))

(restas:define-route retrieved ("zlorec")
  (list :lastid  (max-message-id)
	:lastbad (max-message-id :classes '(bad-message))
	:msgnum  (message-count)
	:badnum  (message-count :classes '(bad-message))))
