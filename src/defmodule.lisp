(restas:define-module #:zlorec
    (:use #:cl #:postmodern #:drakma #:cl-ppcre #:local-time #:parse-html #:iterate #:lweb #:alexandria)
  (:export #:retrieve-loop
	   #:*wait-on-timeout*
	   #:*wait-after-block*
	   #:*block-size*
	   #:*retrieve-run-time*
	   #:message
	   #:find-last-id
	   #:max-message-id))

(restas:define-module #:zlorec-daemon
    (:use #:cl #:iter #:alexandria)
  (:export))
