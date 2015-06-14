;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:dragons
  (:use #:cl)
  (:nicknames #:dns)
  (:export #:query
	   #:iquery
           #:question
	   #:answer
	   #:*dns-host*
	   #:dns-error

           ;; for resource record access
           #:rr-name
           #:rr-type
           #:rr-class
           #:rr-ttl
           #:rr-rdata
	   
	   ;; database 
	   #:*database-path*
	   #:list-records
	   #:find-record
	   #:remove-record
	   #:add-record

	   ))
