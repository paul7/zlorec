(in-package #:zlorec)

(setf *default-render-method*
      #'(lambda (obj)
	  (closure-template.standard:xhtml-strict-frame
	   (list :title (getf obj :title)
		 :body (zlorec.view:main-view 
			(list 
			 :impl (list :type (lisp-implementation-type)
				     :version (lisp-implementation-version))
			 :body (restas:render-object 
				(find-package ':zlorec.view)
				obj)))))))

(restas:define-route main ("")
  (restas:redirect 'graph-form))

(restas:define-route graph-form ("graph-form")
  (list :graphurl (restas:genurl 'graph)))

(restas:define-route graph ("graph"
			    :method :post
			    :requirement #'(lambda () (hunchentoot:post-parameter "send")))
  (list :graph (restas:genurl 'user-activity 
			      :user (hunchentoot:post-parameter "user")
			      :unit (hunchentoot:post-parameter "unit")
			      :amount (hunchentoot:post-parameter "amount"))
	:return (restas:genurl 'graph-form)))

(defun validate-unit (unit)
  (cond 
    ((equalp unit "day")   :day)
    ((equalp unit "month") :month)
    ((equalp unit "year")  :year)))

(defparameter *graph-height* 400)
(defparameter *graph-width* 400)

(restas:define-route user-activity ("useract.svg/:user/:unit/:amount"
				    :parse-vars (list :amount #'parse-integer
						      :unit #'validate-unit)
				    :render-method #'zlorec.view:user-activity
				    :content-type "image/svg+xml")
  (if (and (plusp (length user))
	   (plusp amount))
      (let* ((values (get-user-activity user :unit unit :amount amount))
	     (max-value (apply #'max values))
	     (bar-width (round (/ *graph-width* amount)))
	     (offset -1)
	     (scaled (mapcar #'(lambda (val)
				 (list :y (round (* (* val 0.9) (/ *graph-height* max-value)))
				       :x bar-width
				       :offset (* (incf offset) bar-width)))
			     values)))
	(list :values scaled
	      :width *graph-width*
	      :height *graph-height*))
      (list :error "bad parameters")))
		      


