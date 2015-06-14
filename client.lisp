;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package #:dragons)


(define-condition dns-error (error)
  ((stat :initarg :stat :reader dns-error-stat))
  (:report (lambda (c stream)
	     (format stream "DNS-ERROR: ~A" (dns-error-stat c)))))
	       
;; -----------------------------

(defconstant +dns-port+ 53)

(defun query-udp (host message &key (timeout 1))
  (let ((socket (usocket:socket-connect host +dns-port+
					:protocol :datagram
					:element-type '(unsigned-byte 8))))
    (let ((buffer
	   (flexi-streams:with-output-to-sequence (s)
	     (encode-message s message)))) 
      (usocket:socket-send socket buffer (length buffer)))
    (when (usocket:wait-for-input socket 
				  :timeout timeout
				  :ready-only t)
      (let ((buffer (nibbles:make-octet-vector 512))) ;; 512 is the maximum allowed udp packet size for DNS queries
	(multiple-value-bind (%buffer count remote-host remote-port) (usocket:socket-receive socket buffer 512)
	  (declare (ignore %buffer remote-host remote-port))
;;	  (format t "~A:~A ~A ~%~A~%" remote-host remote-port count (subseq buffer 0 count))
	  (when (or (< count 0) (= count #xffffffff)) 
	    ;; this is a workaround for behaviour I've seen in SBCL-Win32-x64 and LispWorks
	    (error "socket-receive error"))
	  (flexi-streams:with-input-from-sequence (s buffer :end count)
	    (let ((*resolve-pointer-hook* 
		   (lambda (offset)
		     (flexi-streams:with-input-from-sequence (v buffer :start offset)
		       (decode-name v)))))
	      (decode-message s))))))))

(defun query-tcp (host message &key (timeout 1))
  (let ((socket (usocket:socket-connect host +dns-port+
					:protocol :stream
					:element-type '(unsigned-byte 8))))
    (let ((buffer
	   (flexi-streams:with-output-to-sequence (s)
	     (encode-message s message))))
      (nibbles:write-ub16/be (length buffer) (usocket:socket-stream socket))
      (write-sequence buffer (usocket:socket-stream socket))
      (force-output (usocket:socket-stream socket)))
    (when (usocket:wait-for-input socket 
				  :timeout timeout
				  :ready-only t)
      (let* ((len (nibbles:read-ub16/be (usocket:socket-stream socket)))
	     (buffer (nibbles:make-octet-vector len)))
	(read-sequence buffer (usocket:socket-stream socket))
	(flexi-streams:with-input-from-sequence (s buffer)
	  (let ((*resolve-pointer-hook* 
		 (lambda (offset)
		   (flexi-streams:with-input-from-sequence (v buffer :start offset)
		     (decode-name v)))))
	  (decode-message s)))))))


(defvar *dns-host* nil
  "Address of default DNS.")

(defun query (questions &key host (timeout 1) (protocol :udp))
  "Send a DNS query to the DNS specified by HOST or *DNS-HOST*.

QUESTIONS ::= either a list of questions, as returned by QUESTION, a single question or a string, which is interpreted as a standard :A/:IN question.
TIMEOUT ::= time to wait for a reply.
PROTOCOL ::= protocol to send the quest, :UDP or :TCP.

Returns (values answer* authority* addtitional* question*) with
ANSWER, AUTHORITY, ADDITIONAL ::= resource record (RR) instances
QUESTION ::= question instance.

The resource records contain different data depending on the type/class of resource, access the RDATA slot for the data. 
"
  (cond
    ((null *dns-host*)
     (if host
	 (setf *dns-host* host)
	 (error "Must provide a DNS address or set *DNS-HOST*")))
    ((null host) 
     (setf host *dns-host*)))

  ;; allow questions to be specified by string
  (cond 
    ((stringp questions) 
     ;; a string was specified as the question. assume it's an address query
     (setf questions (list (question questions))))    
    ((and (listp questions) (not (listp (car questions))))
     ;; a single questions was provided. wrap in a list 
     (setf questions (list questions))))
    
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
      (break)
      (return-from query answers)))


  (let ((qmessage (message questions)))
    (let ((message 
	   (ecase protocol
	     (:udp (query-udp host qmessage :timeout timeout))
	     (:tcp (query-tcp host qmessage :timeout timeout)))))
      (unless message
	(error "Request timed out"))
      ;; if the header stat is not OK then an error occured
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
	      (message-questions message)))))

(defun answer (rdata &optional (type :a) (class :in))
  (make-rr :name ""
	   :type type
	   :class class
	   :ttl 0
	   :rdata rdata))

(defun iquery (answers &key host (timeout 1) (protocol :udp))
  "Send a DNS inverse query to the DNS specified by HOST or *DNS-HOST*.

ANSWERS ::= a list of answers, as returned by ANSWER.
TIMEOUT ::= time to wait for a reply.
PROTOCOL ::= protocol to send the quest, :UDP or :TCP.

Returns (values answer* authority* addtitional* question*) with
ANSWER, AUTHORITY, ADDITIONAL ::= resource record (RR) instances
QUESTION ::= question instance.

The resource records contain different data depending on the type/class of resource, access the RDATA slot for the data. 
"
  (cond
    ((null *dns-host*)
     (if host
	 (setf *dns-host* host)
	 (error "Must provide a DNS address or set *DNS-HOST*")))
    ((null host) 
     (setf host *dns-host*)))

  (when (rr-p answers) 
    (setf answers (list answers)))
    
  (let ((qmessage (message nil :opcode :iquery 
			   :answers answers)))
    (let ((message 
	   (ecase protocol
	     (:udp (query-udp host qmessage :timeout timeout))
	     (:tcp (query-tcp host qmessage :timeout timeout)))))
      (unless message
	(error "Request timed out"))
      ;; if the header stat is not OK then an error occured
      (unless (eq (header-rcode (message-header message)) :ok)
	(error 'dns-error :stat (header-rcode (message-header message))))
      (values (message-answers message)
	      (message-authorities message)
	      (message-additionals message)
	      (message-questions message)))))



