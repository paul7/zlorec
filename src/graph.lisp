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

(defparameter *graph-height* 500)
(defparameter *graph-width* 500)
(defparameter *graph-sections* 5)
(defparameter *graph-border-part* 1/10)

(defun scale-unit (max-value)
  (let ((ten-power (max 1 (expt 10 
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
	 (scaled (iter 
		   (for offset from 0)
		   (for val in values)
		   (for sub in subscripts)
		   (collect
		       (list :y         (floor (* val scale))
			     :x         bar-width
			     :offset    (+ border-width (* offset bar-width))
			     :bottom    (+ border-height height)
			     :subscript sub))
			 values))
	 (marks (iter 
		  (for mark from 1 below *graph-sections*)
		  (collect 
		      (let ((mark (* scale-unit mark)))
			(list :mark mark
			      :y    (- *graph-height* border-height (* scale mark))))))))
    (print subscripts)
    (list :values  scaled
	  :marks   marks
	  :bwidth  border-width
	  :bheight border-height
	  :width   (+ (* 2 border-width)  (* amount bar-width))
	  :height  *graph-height*
	  :title   title)))

(pm:define-memoized-route user-activity ("useract.svg/:user/:unit/:amount"
					 :parse-vars (list :amount #'parse-integer
							   :unit   #'validate-unit
							   :user   #'hunchentoot:url-decode)
					 :render-method #'zlorec.view:svg-bar-graph
					 :content-type "image/svg+xml")
  (if (and (plusp (length user))
	   (plusp amount))
      (values 
       (render-svg-bar-graph (get-user-activity user :unit unit :amount amount) 
			     :title      user
			     :subscripts (iota amount :start (- amount))) 
       '(1 :sec))
      (list :error "bad parameters")))

(restas:define-route retrieved ("zlorec")
  (list :lastid  (max-message-id)
	:lastbad (max-message-id :classes '(bad-message))
	:msgnum  (message-count)
	:badnum  (message-count :classes '(bad-message))))
