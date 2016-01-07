;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:dragons)


(define-condition dns-error (error)
  ((stat :initarg :stat :reader dns-error-stat))
  (:report (lambda (c stream)
             (format stream "DNS-ERROR: ~A" (dns-error-stat c)))))

;; -----------------------------

(defconstant +dns-port+ 53)

(defun query-udp (addr message &optional timeout)
  (declare (type fsocket:sockaddr-in addr))
  (let ((fd (fsocket:open-socket :type :datagram))
        (pc (fsocket:open-poll))
        (buf (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
    (unwind-protect
         (progn           
           (fsocket:socket-bind fd (fsocket:make-sockaddr-in))
           (fsocket:poll-register pc
                                  (make-instance 'fsocket:pollfd
                                                 :fd fd
                                                 :events (fsocket:poll-events :pollin)))
           (fsocket:socket-sendto fd
                                  (flexi-streams:with-output-to-sequence (s)
                                    (encode-message s message))
                                  addr)
           (unless (fsocket:poll pc :timeout (or timeout 500))
             (error 'dns-error :stat "Timeout"))
           
           (multiple-value-bind (cnt raddr) (fsocket:socket-recvfrom fd buf)
             (declare (ignore raddr))
             (flexi-streams:with-input-from-sequence (s buf :end cnt)
               (let ((*resolve-pointer-hook* 
                      (lambda (offset)
                        (flexi-streams:with-input-from-sequence (v buf :start offset)
                          (decode-name v)))))
                 (decode-message s)))))
      (fsocket:close-socket fd)
      (fsocket:close-poll pc))))

(defun query-tcp (addr message &optional timeout)
  (declare (type fsocket:sockaddr-in addr))
  (let ((fd (fsocket:open-socket :type :stream))
        (pc (fsocket:open-poll))
        (buf (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
    (unwind-protect
         (progn           
           (fsocket:socket-bind fd (fsocket:make-sockaddr-in))
           (fsocket:socket-connect fd addr)
           (fsocket:poll-register pc
                                  (make-instance 'fsocket:pollfd
                                                 :fd fd
                                                 :events (fsocket:poll-events :pollin)))
           ;; TODO: check for short write 
           (fsocket:socket-send fd
                                (flexi-streams:with-output-to-sequence (s)
                                  (encode-message s message)))
           ;; Read a uint32 byte count followed by the message
           (let ((mcount 0))
             (unless (fsocket:poll pc :timeout (or timeout 500))
               (error 'dns-error :stat "Timeout"))
             (fsocket:socket-recv fd buf :end 4)
             ;; TODO: check for short read
             (setf mcount (nibbles:ub32ref/be buf 0))
             (do ((cnt 0))
                 ((= cnt mcount))
               (unless (fsocket:poll pc :timeout (or timeout 500))
                 (error 'dns-error "Timeout"))
               (let ((rcnt (fsocket:socket-recv fd buf :start (+ cnt 4))))
                 (incf cnt rcnt)))

             ;; parse the message 
             (flexi-streams:with-input-from-sequence (s buf :end mcount)
               (let ((*resolve-pointer-hook* 
                      (lambda (offset)
                        (flexi-streams:with-input-from-sequence (v buf :start offset)
                          (decode-name v)))))
                 (decode-message s)))))
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
      (unless message (error 'dns-error "Timeout"))

      (process-message message))))


(defun query-db (questions)
  ;; first try and answer the query using the local database
  (let ((answers
         (loop :for question :in questions
            :nconc
            (let ((rr (find-record (make-rr :name (getf question :name)
                                            :type (getf question :type)
                                            :class (getf question :class)
                                            :ttl 0
                                            :rdata nil))))
              (when rr (list rr))))))
    (when answers
      (return-from query-db answers))))

(defun query-multiple (questions addresses &optional timeout)
  ;; allocate socket and poll decscriptor
  (let* ((fd (fsocket:open-socket))
	 (pc (fsocket:open-poll))
	 (msgs (mapcar (lambda (addr)
			 (let ((id (random 65536)))
			   (list addr 
				 (flexi-streams:with-output-to-sequence (s)
				   (encode-message s (message questions :id id)))
				 id)))
		       addresses)))
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
	  (raddress nil))
      (do ((buffer (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0))
	   (done nil))
	  ((or ranswers done))
	(if (fsocket:poll pc :timeout timeout)
	    (multiple-value-bind (cnt raddr) (fsocket:socket-recvfrom fd buffer)
	      
	      (let ((rmsg 
		     (flexi-streams:with-input-from-sequence (s buffer :end cnt)
		       (let ((*resolve-pointer-hook* 
			      (lambda (offset)
				(flexi-streams:with-input-from-sequence (v buffer :start offset)
				  (decode-name v)))))
			 (decode-message s)))))
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
      
      (values ranswers rauths radds rqs raddress))))
  
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


;; ------------------- Inverse queries -----------------------------

(defun answer (rdata &optional (type :a) (class :in))
  (make-rr :name ""
           :type type
           :class class
           :ttl 0
           :rdata rdata))

(defun iquery (answers &key addr timeout (protocol :udp))
  "Send a DNS inverse query to the DNS specified by ADDR or the first of *DNS-ADDRS*.
ANSWERS ::= a list of answers, as returned by ANSWER.
TIMEOUT ::= time to wait for a reply.
PROTOCOL ::= protocol to send the quest, :UDP or :TCP.
Returns (values answer* authority* addtitional* question*) with
ANSWER, AUTHORITY, ADDITIONAL ::= resource record (RR) instances
QUESTION ::= question instance.
The resource records contain different data depending on the type/class of resource, access the RDATA slot for the data. 
"
  (when (rr-p answers) 
    (setf answers (list answers)))
  
  (let ((qmessage (message nil :opcode :iquery 
                           :answers answers)))
    (let ((message 
           (ecase protocol
             (:udp (query-udp (or addr (car *dns-addr*)) qmessage timeout))
             (:tcp (query-tcp (or addr (car *dns-addr*)) qmessage timeout)))))
      (unless message (error 'dns-error :stat "Timeout"))
      ;; if the header stat is not OK then an error occured
      (unless (eq (header-rcode (message-header message)) :ok)
        (error 'dns-error :stat (header-rcode (message-header message))))
      (values (message-answers message)
              (message-authorities message)
              (message-additionals message)
              (message-questions message)))))


;; ------------------- Useful wrappers ---------------------

(defun get-host-by-name (name)
  "Resolve a hostname into a list of SOCKADDR-IN addresses.
NAME ::= string hostname.
Returns a list of FSOCKET:SOCKADDR-IN addresses."
  (declare (type string name))
  (mapcar (lambda (rr)
            (fsocket:make-sockaddr-in :addr (rr-rdata rr)))
          (query (question name))))

(defun get-host-by-addr (addr)
  "Resolve an internet address into a list of hostnames.
ADDR ::= either a string representing a dotted quad or a FSOCKET:SOCKADDR-IN address
Returns a list of strings for known hostnames."
  (etypecase addr
    (string (setf addr (fsocket:make-sockaddr-in :addr (fsocket::dotted-quad-to-inaddr addr))))
    (fsocket:sockaddr-in nil))
  (let ((dq (format nil "~A.~A.~A.~A"
                    (aref (fsocket:sockaddr-in-addr addr) 3)
                    (aref (fsocket:sockaddr-in-addr addr) 2)
                    (aref (fsocket:sockaddr-in-addr addr) 1)
                    (aref (fsocket:sockaddr-in-addr addr) 0))))
    (mapcar (lambda (rr)
              (rr-rdata rr))
            (query (question (format nil "~A.in-addr.arpa" dq)
                             :ptr)))))

