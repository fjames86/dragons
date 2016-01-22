;;;; Copyright (c) Frank James 2016 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:dragons-server
  (:use #:cl #:dragons)
  (:import-from #:dragons
		#:encode-message
		#:decode-message
		#:message
		#:message-answers
		#:message-questions
		#:message-authorities
		#:message-additionals
		#:xdr-block
		#:xdr-block-offset
		#:xdr-block-buffer
		#:xdr-block-count
		#:reset-xdr-block
		#:header-id
		#:message-header
		#:+dns-port+
		#:header-rd-p
		#:header-qr
		#:header-opcode
		#:make-rr)
  (:export #:defzone))

(in-package #:dragons-server)

;;; This file is an attempt to define a recursive name server

;; --------------------- Debug logging -------------

(defvar *log* nil)

(defun open-log ()
  (unless *log*
    (setf *log* (pounds.log:open-log :path
				     (merge-pathnames "dragons.log"
						      (user-homedir-pathname))
				     :tag "DNS "))))

(defun close-log ()
  (when *log*
    (pounds.log:close-log *log*)
    (setf *log* nil)))

(defun dns-log (lvl format-string &rest args)
  (when *log*
    (pounds.log:write-message *log* lvl
			      (apply #'format nil format-string args))))

;; --------------------- Zone files ------------------------

(defvar *zones* nil)

(defun %defzone (zone)
  (destructuring-bind (zname &rest zrrs) zone
    (dolist (z *zones*)
      (when (string-equal (car z) zname)
	(setf (cdr z) zrrs)
	(return-from %defzone z))))
  (push zone *zones*)
  zone)

;; Each zone for which this server is authoritative is simply an alist of RR instances.
;; This can be defined in e.g. a set of Lisp files.
(defmacro defzone (name options &rest rrs)
  (declare (ignore options))
  (let ((gzone (gensym "zone")))
    `(let ((,gzone 
	    (list ,(string name)
		  ,@(mapcar (lambda (rr)
			      (destructuring-bind (rr-name rr-type rr-class rr-ttl rr-data) rr
				`(make-rr :name ,(string rr-name)
					  :type ',rr-type
					  :class ',rr-class
					  :ttl ,rr-ttl
					  :rdata ',rr-data)))
			    rrs))))
       (%defzone ,gzone))))

;; e.g. 
(defzone frank.com ()
  (frank.com :soa :in 123 (:mname "" :rname "" :serial 0 :refresh 0 :retry 0 :expire 0 :minimum 0))
  (a.frank.com :a :in 123 #(1 2 3 4))
  (4.3.2.1.in-addr.arpa :ptr :in 123 "a.frank.com"))

(defun find-zone (name)
  (cdr (assoc name *zones* :test #'string-equal)))


;; (defun genzone (zone)
;;   (destructuring-bind (zname &rest rrs) zone 
;;     (with-open-file (f (format nil "~A.txt" zname)
;; 		       :direction :output :if-exists :supersede)
;;       (dolist (rr rrs)
;; 	(format f "~A~50T~A~5T~A~5T~A~%"
;; 		(rr-name rr) (rr-type rr) (rr-class rr) (rr-ttl rr))))))

(defun find-zone-records (name type class)
  (let (rrs)
    (dolist (zone *zones*)
      (dolist (rr (cdr zone))
	(when (and (string-equal (rr-name rr) name)
		   (eq (rr-type rr) type)
		   (eq (rr-class rr) class))
	  (push rr rrs))))
    rrs))

;; ---------------------- Additional record processing ---------------

;; When answering a query we sometimes need to also return addional records.
;; This section defines how to do that.

;; store a list of query types and the additional records they should return for them.
;; E.g. when querying NS records you must also return additional A records.
(defvar *additional-records*
  '((:ns :a :in)
    (:srv :a :in)))

(defun find-name-additionals (name type)
  "Returns additional records for queries.
NAME ::= query record name
TYPE ::= query type.
Returns a list of RR structs for each additional record that should be returned."
  (let ((arrs nil))
    (dolist (add *additional-records*)
      (destructuring-bind (qtype atype aclass) add
	(when (eq type qtype)
	  (setf arrs
		(append arrs (find-zone-records name atype aclass))))))
    arrs))

;; ---------------------- Server ----------------------

(defstruct call
  id
  timeout
  cb
  cxt)

(defstruct server
  exiting
  thread
  calls
  (blk (xdr-block 512))
  fd
  pc
  raddr)

    
(defun get-internal-real-time-msecs ()
  (truncate (* (get-internal-real-time) 1000)
	    internal-time-units-per-second))

(defun pop-call (server id)
  (do ((calls (server-calls server) (cdr calls))
       (prev nil)
       (res nil))
      ((null calls) res)
    (when (= (call-id (car calls)) id)
      ;; found it
      (let ((call (car calls)))
	(if prev
	    (setf (cdr prev) (cdr calls))
	    (setf (server-calls server) (cdr (server-calls server))))
	(setf res call)))
    (setf prev calls)))

(defun purge-calls (server)
  (do ((calls (server-calls server) (cdr calls))
       (prev nil)
       (now (get-internal-real-time-msecs)))
      ((null calls))
    ;; purge any which have timed out
    (let ((call (car calls)))
      (when (> now (call-timeout call))
	(dns-log :debug "[~A] Purging" (call-id call))
	(if prev
	    (setf (cdr prev) (cdr calls))
	    (setf (server-calls server) (cdr (server-calls server))))
	(funcall (call-cb call) server (call-cxt call) nil)))
    (setf prev calls)))

(defun await-reply (server id cb cxt &optional (timeout 500))
  (let ((tt (+ (get-internal-real-time-msecs) timeout)))
    (dns-log :debug "[~A] Await reply for ~Ams" id timeout)
    
    (push (make-call :id id
		     :cb cb
		     :cxt cxt
		     :timeout tt)
	  (server-calls server))))

(defun reply-callback (server cxt msg)
  (destructuring-bind (id raddr) cxt 
    (cond
      (msg
       ;; We have received a reply. If it contains an answer then respond with that back tothe client.
       ;; If not then send a query to the next name server.
       ;; If no name servers left to try then either be silent or reply with an error?
       (let ((ans (message-answers msg)))
	 (cond
	   (ans
	    ;; we got some answers, send them back to the original client
	    (dns-log :debug "[~A] Received answers"
		     (header-id (message-header msg)))
	    (let ((blk (server-blk server))
		  (rmsg (message (message-questions msg)
				 :qr :reply
				 :answers ans
				 :authorities (message-authorities msg)
				 :additionals (message-additionals msg)
				 :id id
				 :rd-p nil
				 :ra-p t)))
	      (reset-xdr-block blk)
	      (encode-message blk rmsg)
	      (fsocket:socket-sendto (server-fd server)
				     (xdr-block-buffer blk)
				     raddr
				     :start 0 :end (xdr-block-offset blk))))
	   (t
	    ;; no answers, try calling the next nameserver which should be
	    ;; listed in the authorities
	    (dns-log :debug "[~A] No answers"
		     (header-id (message-header msg)))
	    (let* ((auths (message-authorities msg))
		   (adds (message-additionals msg))
		   (ns (find :ns auths :key #'rr-type))
		   (nsadd (find (rr-rdata ns) adds
				:test #'string-equal
				:key #'rr-name))
		   (rmsg (message (message-questions msg)
				  :qr :query :opcode :query :rd-p nil))
		   (blk (server-blk server)))
	      (when nsadd
		(reset-xdr-block blk)
		(encode-message blk rmsg)
		(fsocket:socket-sendto (server-fd server)
				       (xdr-block-buffer blk)
				       (fsocket:sockaddr-in (rr-rdata nsadd) +dns-port+)
				       :start 0 :end (xdr-block-offset blk))
		(await-reply server (header-id (message-header rmsg))
			     #'reply-callback (list id raddr)))
		
		nil)))))
      (t
       ;; timedout waiting for a reply. Be silent or send an error reply?
       (dns-log :debug "[~A] Timeout waiting" id)
       (let ((msg (message nil
			   :qr :reply
			   :id id 
			   :opcode :query 
			   :rcode :name-error
			   :ra-p t))
	     (blk (server-blk server)))
	 (reset-xdr-block blk)
	 (encode-message blk msg)
	 (fsocket:socket-sendto (server-fd server)
				(xdr-block-buffer blk)
				raddr
				:start 0 :end (xdr-block-offset blk)))))))


(defun process-reply (server msg)
  "We have received a reply. Look up a matching call from the ID."
  (dns-log :debug "[~A] REPLY"
	   (header-id (message-header msg)))
  (let ((call (pop-call server (header-id (message-header msg)))))
    (when call
      ;; execute the callback
      (funcall (call-cb call) server (call-cxt call) msg))))

;; I got these from IANA
;; Hostname	IP Addresses	Manager
;; a.root-servers.net	198.41.0.4, 2001:503:ba3e::2:30	VeriSign, Inc.
;; b.root-servers.net	192.228.79.201, 2001:500:84::b	University of Southern California (ISI)
;; c.root-servers.net	192.33.4.12, 2001:500:2::c	Cogent Communications
;; d.root-servers.net	199.7.91.13, 2001:500:2d::d	University of Maryland
;; e.root-servers.net	192.203.230.10	NASA (Ames Research Center)
;; f.root-servers.net	192.5.5.241, 2001:500:2f::f	Internet Systems Consortium, Inc.
;; g.root-servers.net	192.112.36.4	US Department of Defense (NIC)
;; h.root-servers.net	198.97.190.53, 2001:500:1::53	US Army (Research Lab)
;; i.root-servers.net	192.36.148.17, 2001:7fe::53	Netnod
;; j.root-servers.net	192.58.128.30, 2001:503:c27::2:30	VeriSign, Inc.
;; k.root-servers.net	193.0.14.129, 2001:7fd::1	RIPE NCC
;; l.root-servers.net	199.7.83.42, 2001:500:3::42	ICANN
;; m.root-servers.net	202.12.27.33, 2001:dc3::35	WIDE Project

(defvar *root-name-servers*
  (mapcar (lambda (rns)
	    (fsocket:sockaddr-in (car rns) +dns-port+))
	  '((#(198 41 0 4) "a.root-servers.net")
	    (#(192 228 79 201) "b.root-servers.net") 
	    (#(192 33 4 12) "c.root-servers.net") 
	    (#(199 7 91 13) "d.root-servers.net") 
	    (#(192 203 230 10) "e.root-servers.net") 
	    (#(192 5 5 241) "f.root-servers.net") 
	    (#(192 112 36 4) "g.root-servers.net") 
	    (#(198 36 148 17) "h.root-servers.net") 
	    (#(192 58 128 30) "j.root-servers.net")
	    (#(193 0 14 129) "k.root-severs.net") 
	    (#(199 7 83 42) "l.root-servers.net")
	    (#(202 12 27 33) "m.root-servers.net"))))


(defun process-query (server msg)
  "We have received a query request. We lookup in our database.
If we find it we return the result immediately. If we don't find it and the 
request was for a recursive query, we must initiate a recursive series of calls
starting at the top level domain. Otherwise we can return immediately with
some authority name servers."
  (let ((questions (message-questions msg))	(answers nil))
    (dolist (q questions)
      (destructuring-bind (&key name type class) q
	(dns-log :debug "[~A] QUERY ~A ~A ~A"
		 (header-id (message-header msg))
		 name type class)

	(let ((rrs (find-zone-records name type class)))
	  (cond
	    (rrs
	     (setf answers (append answers rrs)))
	    ((not (header-rd-p (message-header msg)))
	     ;; recursion not desired
	     nil)
	    (t
	     ;; recursion desired : send a query to the root servers and
	     ;; return immediately here
	     (reset-xdr-block (server-blk server))
	     (let ((smsg (message (list q) :rd-p nil))
		   (rs (nth (random (length *root-name-servers*))
			    *root-name-servers*)))
	       (dns-log :debug "[~A] CALL RS ~A"
			(header-id (message-header smsg))
			(fsocket:sockaddr-in-addr rs))
	       (encode-message (server-blk server) smsg)
	       (fsocket:socket-sendto (server-fd server)
				      (xdr-block-buffer (server-blk server))
				      rs
				      :start 0 :end (xdr-block-offset (server-blk server)))
	       (await-reply server (header-id (message-header smsg))
			    #'reply-callback 
			    (list (header-id (message-header msg)) (server-raddr server)) 1000)))))))
    
    ;; send an authoritative reply with the answers (if any)
    (when answers
      (let ((amsg (message questions
			   :qr :reply
			   :answers answers
			   :aa-p t
			   :ra-p t
			   :rd-p (header-rd-p (message-header msg))
			   :id (header-id (message-header msg))))
	    (blk (server-blk server)))
	(reset-xdr-block blk)
	(encode-message blk amsg)
	(fsocket:socket-sendto (server-fd server)
			       (xdr-block-buffer blk)
			       (server-raddr server)
			       :start 0 :end (xdr-block-offset blk))))))

(defun process-request (server)
  (let* ((blk (server-blk server))
	 (msg (decode-message blk)))
    ;; TODO: use opcode and support :status calls
    (dns-log :debug "[~A] ~A MSG ~S ~S"
	     (header-id (message-header msg))
	     (fsocket:sockaddr-in-addr (server-raddr server))
	     (header-opcode (message-header msg))
	     (header-qr (message-header msg)))
    (case (header-opcode (message-header msg))
      (:query
       (ecase (header-qr (message-header msg))
	 (:query (process-query server msg))
	 (:reply (process-reply server msg))))
      (otherwise
       ;; unknown or unsupported opcode is ignored
       nil))))


;; TODO: support TCP 
(defun run-server (server)
  (let ((fd (fsocket:open-socket))
	(pc (fsocket:open-poll)))
    (fsocket:socket-bind fd (fsocket:sockaddr-in #(0 0 0 0) 53))
    (fsocket:poll-register pc
			   (make-instance 'fsocket:pollfd
					  :fd fd
					  :events (fsocket:poll-events :pollin)))
    (setf (server-fd server) fd
	  (server-pc server) pc)
    
    (unwind-protect
	 (do ((blk (server-blk server)))
	     ((server-exiting server))
	   (handler-case
	       (progn
		 ;; TODO: when supporting TCP we need to define our pollfds and
		 ;; change this to a DOEVENTS iteration
		 (when (fsocket:poll pc :timeout 1000)
		   (multiple-value-bind (count raddr)
		       (fsocket:socket-recvfrom fd (xdr-block-buffer blk))
		     (setf (xdr-block-offset blk) 0
			   (xdr-block-count blk) count
			   (server-raddr server) raddr)
		     (process-request server)))
		 
		 ;; purge any outstaning calls
		 (purge-calls server))
	     (error (e)
	       (dns-log :error "~A" e))))
      (fsocket:close-socket fd)
      (fsocket:close-poll pc))))

(defvar *server* nil)

(defun start-server ()
  (unless *server*
    (setf *server* (make-server))
    (setf (server-thread *server*)
	  (bt:make-thread (lambda () (run-server *server*))))))

(defun stop-server ()
  (when *server*
    (setf (server-exiting *server*) t)
    (bt:join-thread (server-thread *server*))
    (setf *server* nil)))
