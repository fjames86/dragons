;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:dragons)


(define-condition dns-error (error)
  ((stat :initform nil :initarg :stat :reader dns-error-stat))
  (:report (lambda (c stream)
             (format stream "DNS-ERROR: ~A" (dns-error-stat c)))))

;; -----------------------------

(defconstant +dns-port+ 53)

(defun query-udp (addr message &optional timeout)
  (declare (type fsocket:sockaddr-in addr))
  (let ((fd (fsocket:open-socket :type :datagram))
        (pc (fsocket:open-poll))
        (blk (xdr-block 512)))
    (unwind-protect
         (progn
	   (encode-message blk message)
	   
           (fsocket:socket-bind fd (fsocket:make-sockaddr-in))
           (fsocket:poll-register pc
                                  (make-instance 'fsocket:pollfd
                                                 :fd fd
                                                 :events (fsocket:poll-events :pollin)))
           (fsocket:socket-sendto fd
				  (xdr-block-buffer blk)
                                  addr
				  :start 0 :end (xdr-block-offset blk))
           (unless (fsocket:poll pc :timeout (or timeout 500))
             (error 'dns-error :stat "Timeout"))
           
           (multiple-value-bind (cnt raddr) (fsocket:socket-recvfrom fd (xdr-block-buffer blk))
             (declare (ignore raddr))
	     (setf (xdr-block-offset blk) 0
		   (xdr-block-count blk) cnt)
	     (decode-message blk)))
      (fsocket:close-socket fd)
      (fsocket:close-poll pc))))

(defun query-tcp (addr message &optional timeout)
  (declare (type fsocket:sockaddr-in addr))
  (let ((fd (fsocket:open-socket :type :stream))
        (pc (fsocket:open-poll))
        (blk (xdr-block 512)))
    (unwind-protect
         (progn
	   (encode-message blk message)
	   
           (fsocket:socket-bind fd (fsocket:make-sockaddr-in))
           (fsocket:socket-connect fd addr)
           (fsocket:poll-register pc
                                  (make-instance 'fsocket:pollfd
                                                 :fd fd
                                                 :events (fsocket:poll-events :pollin)))
           ;; TODO: check for short write
	   (let ((v (xdr-block 4)))
	     (encode-uint32 blk (xdr-block-offset blk))
	     (fsocket:socket-send fd (xdr-block-buffer v)))
           (fsocket:socket-send fd (xdr-block-buffer blk)
				:start 0 :end (xdr-block-offset blk))

           ;; Read a uint32 byte count followed by the message
           (let ((mcount 0))
             (unless (fsocket:poll pc :timeout (or timeout 500))
               (error 'dns-error :stat "Timeout"))
             (fsocket:socket-recv fd (xdr-block-buffer blk) :end 4)
             ;; TODO: check for short read
             (setf mcount (nibbles:ub32ref/be (xdr-block-buffer blk) 0))
             (do ((cnt 0))
                 ((= cnt mcount))
               (unless (fsocket:poll pc :timeout (or timeout 500))
                 (error 'dns-error :stat "Timeout"))
               (let ((rcnt (fsocket:socket-recv fd (xdr-block-buffer blk)
						:start cnt)))
                 (incf cnt rcnt)))
	     (setf (xdr-block-offset blk) 0
		   (xdr-block-count blk) mcount)
	     (decode-message blk)))
      (fsocket:close-socket fd)
      (fsocket:close-poll pc))))


(defvar *dns-addrs* (fsocket:get-name-servers)
  "Address of default DNS.")

(defun process-message (message)
  (unless (eq (header-rcode (message-header message)) :ok)
    (error 'dns-error :stat (header-rcode (message-header message))))
  
  ;; cache the answers for future use
  (let ((now (get-universal-time)))
    (dolist (rr (message-answers message))
      (unless (zerop (rr-ttl rr))
	;; cache the record
	(add-record rr (+ now (rr-ttl rr))))))
  
  (values (message-answers message)
	  (message-authorities message)
	  (message-additionals message)
	  (message-questions message)))

(defun query* (questions addr &key timeout (protocol :udp))
  "Send a DNS query to the DNS specified by HOST or *DNS-HOST*.

QUESTIONS ::= either a list of questions, as returned by QUESTION, a single question or a string, which is interpreted as a standard :A/:IN question.
ADDR ::= address to send query to.
TIMEOUT ::= time to wait for a reply in milliseconds
PROTOCOL ::= protocol to send the quest, :UDP or :TCP.

Returns (values answer* authority* addtitional* question*) with
ANSWER, AUTHORITY, ADDITIONAL ::= resource record (RR) instances
QUESTION ::= question instance.

The resource records contain different data depending on the type/class of resource, access the RDATA slot for the data. 
"
  (let ((qmessage (message questions)))
    (let ((message 
           (ecase protocol
             (:udp (query-udp addr qmessage timeout))
             (:tcp (query-tcp addr qmessage timeout)))))
      (unless message (error 'dns-error :stat "Timeout"))

      (process-message message))))


(defun query-db (questions)
  ;; first try and answer the query using the local database
  (let (answers)
    (dolist (rr (list-records))
      (dolist (q questions)
	(destructuring-bind (&key name type class) q
	  (when (and (string-equal name (rr-name rr))
		     (eq type (rr-type rr))
		     (eq class (rr-class rr)))
	    (push rr answers)))))
    answers))

(defun query-multiple (questions addresses &optional timeout)
  ;; allocate socket and poll decscriptor

  ;; if no timeout supplied make sure we use one! 
  (unless timeout (setf timeout 500))
  
  (let* ((fd (fsocket:open-socket))
	 (pc (fsocket:open-poll))
	 (msgs (mapcar (lambda (addr)
			 (let ((id (random 65536))
			       (blk (xdr-block 512)))
			   (encode-message blk (message questions :id id))
			   (list addr
				 (subseq (xdr-block-buffer blk)
					 0
					 (xdr-block-offset blk))
				 id)))
		       addresses)))

    (unwind-protect 
	 (progn
	   (fsocket:socket-bind fd (fsocket:make-sockaddr-in))
	   (fsocket:poll-register pc (make-instance 'fsocket:pollfd 
						    :fd fd
						    :events (fsocket:poll-events :pollin)))
	   
	   ;; send to each address
	   (do ((%msgs msgs (cdr %msgs)))
	       ((null %msgs))
	     (destructuring-bind (addr buffer id) (car %msgs)
	       (declare (ignore id))
	       (fsocket:socket-sendto fd buffer addr)))
	   
	   ;; receive 
	   (let ((ranswers nil)
		 (rauths nil)
		 (radds nil)
		 (rqs nil)
		 (got-reply nil)
		 (raddress nil))
	     (do ((blk (xdr-block 512))
		  (done nil))
		 ((or ranswers done))
	       (if (fsocket:poll pc :timeout timeout)
		   (multiple-value-bind (cnt raddr) (fsocket:socket-recvfrom fd (xdr-block-buffer blk))
		     #+nil(format t "~A~%" (subseq (xdr-block-buffer blk) 0 cnt))
		     (setf got-reply t)
		     
		     (setf (xdr-block-offset blk) 0
			   (xdr-block-count blk) cnt)

		     (let ((rmsg (decode-message blk)))
		       
		       ;; TODO: match the id with the address so we know whether we received from the same
		       ;; host that we sent it to
		       (handler-case 
			   (multiple-value-bind (answers auths adds qs) (process-message rmsg)
			     (when answers
			       (setf ranswers answers
				     rauths auths
				     radds adds
				     rqs qs
				     raddress raddr)))
			 (dns-error (e)
			   (declare (ignore e))
			   nil))))
		   (setf done t)))

	     (unless got-reply (error 'dns-error :stat "Timeout"))

	     (values ranswers rauths radds rqs raddress)))
      (fsocket:close-socket fd)
      (fsocket:close-poll pc))))
  
(defun query (questions &key addr timeout (protocol :udp))
  "Send a DNS query to the DNS specified by HOST or *DNS-HOST*.

QUESTIONS ::= either a list of questions, as returned by QUESTION, a single question or a string, which is interpreted as a standard :A/:IN question.
ADDR ::= if supplied will send the query to this address, otherwise will try each address in *DNS-ADDRS*.
TIMEOUT ::= time to wait for a reply in milliseconds
PROTOCOL ::= protocol to send the quest, :UDP or :TCP.

Returns (values answer* authority* addtitional* question* raddr) with
ANSWER, AUTHORITY, ADDITIONAL ::= resource record (RR) instances
QUESTION ::= question instance.
RADDR ::= the nameserver which answered the question. If this is NIL then the answer was resolved from the cache.
The resource records contain different data depending on the type/class of resource, access the RDATA slot for the data. 
"

  ;; allow questions to be specified by string
  (cond 
    ((stringp questions) 
     ;; a string was specified as the question. assume it's an address query
     (setf questions (list (question questions))))    
    ((and (listp questions) (not (listp (car questions))))
     ;; a single questions was provided. wrap in a list 
     (setf questions (list questions))))

  ;; try the database first
  (let ((answers (query-db questions)))
    (when answers 
      (return-from query 
	(values answers nil nil nil nil))))

  (case protocol
    (:udp
     (query-multiple questions 
		     (if addr 
			 (list addr)
			 *dns-addrs*)
		     timeout))
    (:tcp
     (let ((answers nil) 
	   (auths nil)
	   (adds nil)
	   (qus nil)
	   (raddr nil))
       ;; TODO: we currently try each addr in turn. Instead we should send a msg to each at the start
       ;; and then wait for replies.
       (do ((addrs (if addr 
		       (list addr)
		       *dns-addrs*)
		   (cdr addrs)))
	   ((or (null addrs) answers))
	 (handler-case 
	     (multiple-value-bind (ans aus ads qs) 
		 (query* questions 
			 (car addrs)
			 :timeout timeout
			 :protocol protocol)
	       (when ans 
		 (setf answers ans
		       auths aus
		       adds ads
		       qus qs
		       raddr (car addrs))))
	   (dns-error (e) 
	     ;; ignore these
	     (declare (ignore e))
	     nil)))
       (values answers auths adds qus raddr)))))

;; ------------------- Useful wrappers ---------------------

(defvar *default-domain*
  (let ((dstr (nth-value 1 (fsocket:get-host-name))))
    (unless (string= dstr "") dstr))
  "Default domain to append to hostnames when no domain is supplied.")

(defun search-host-names (host)
  (let ((pos (position #\. host :test #'char=)))
    (cond
      (pos (list host))
      (*default-domain*
       (list (concatenate 'string host "." *default-domain*)
             host))
      (t (list host)))))

;; TODO: allow users to provide a set of additional domains to search through
(defun get-host-by-name (host)
  "Resolve a hostname into a list of SOCKADDR-IN addresses.
NAME ::= string hostname. If the hostname does not name a FQDN i.e. if HOST does 
not contain a period character #\. then the local domain name is first appended 
to HOST. If this fails then the search falls back to HOST unchanged. 

Returns a list of 4-octet vectors containing the internet address."
  (etypecase host
    (null
     (list #(127 0 0 1))) ;; nil means localhost
    (string
     (cond
       ((string-equal host "localhost")
        (list #(127 0 0 1)))
       (t 
        (let ((dq (fsocket::dotted-quad-to-inaddr host nil)))
          (if dq
              (list dq)
              ;; not a dotted quad, must be a hostname
              (do ((hostnames (search-host-names host) (cdr hostnames))
                   (r nil))
                  ((null hostnames) r)
                (let ((answers (query (question (car hostnames)))))
                  (setf r 
                        (mapcan (lambda (rr)
                                  (when (member (rr-type rr) '(:a :aaaa))
                                    (list (rr-rdata rr))))
                                answers)
                        hostnames nil))))))))
    (vector
     (list host))
    (fsocket:sockaddr-in
     (list (fsocket:sockaddr-in-addr host)))
    (fsocket:sockaddr-in6
     (list (fsocket:sockaddr-in6-addr host)))))

(defun get-host-by-addr (inaddr)
  "Resolve an internet address into a list of hostnames.
ADDR ::= either a 4-octet internet address or a string containing the dotted-quad 
representation of the internet address.
Returns a list of strings for known hostnames."
  (mapcar (lambda (rr)
	    (rr-rdata rr))
	  (query (question (format nil "~A.in-addr.arpa"
				   (etypecase inaddr
				     (string inaddr)
				     (vector (fsocket::inaddr-to-dotted-quad inaddr))))
			   :ptr))))

