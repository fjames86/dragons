;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:dragons
  (:use #:cl)
  (:import-from #:drx
		#:xdr-block-buffer
		#:xdr-block-offset 
		#:xdr-block-count
		#:make-xdr-block
		#:reset-xdr-block 
		#:xdr-block
		#:encode-uint32
		#:decode-uint32)  
  (:nicknames #:dns)
  (:export #:query
           #:iquery
           #:question
           #:answer
           #:*dns-addrs*
           #:dns-error

           #:get-host-by-name
           #:get-host-by-addr
           
           ;; for resource record access
           #:rr-name
           #:rr-type
           #:rr-class
           #:rr-ttl
           #:rr-rdata
           
           ;; public database operators
           #:*database-path*
           #:list-records
           #:remove-record
           #:insert-record
           #:purge-records
           
           ))
