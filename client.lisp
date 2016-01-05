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


(defvar *dns-addr* (first (fsocket:get-name-servers))
  "Address of default DNS.")

(defun query (questions &key addr timeout (protocol :udp))
  "Send a DNS query to the DNS specified by HOST or *DNS-HOST*.

QUESTIONS ::= either a list of questions, as returned by QUESTION, a single question or a string, which is interpreted as a standard :A/:IN question.
TIMEOUT ::= time to wait for a reply in milliseconds
PROTOCOL ::= protocol to send the quest, :UDP or :TCP.

Returns (values answer* authority* addtitional* question*) with
ANSWER, AUTHORITY, ADDITIONAL ::= resource record (RR) instances
QUESTION ::= question instance.

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
      (return-from query answers)))


  (let ((qmessage (message questions)))
    (let ((message 
           (ecase protocol
             (:udp (query-udp (or addr *dns-addr*) qmessage timeout))
             (:tcp (query-tcp (or addr *dns-addr*) qmessage timeout)))))
      (unless message (error 'dns-error "Timeout"))
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

(defun iquery (answers &key addr timeout (protocol :udp))
  "Send a DNS inverse query to the DNS specified by HOST or *DNS-HOST*.

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
             (:udp (query-udp (or addr *dns-addr*) qmessage timeout))
             (:tcp (query-tcp (or addr *dns-addr*) qmessage timeout)))))
      (unless message (error 'dns-error :stat "Timeout"))
      ;; if the header stat is not OK then an error occured
      (unless (eq (header-rcode (message-header message)) :ok)
        (error 'dns-error :stat (header-rcode (message-header message))))
      (values (message-answers message)
              (message-authorities message)
              (message-additionals message)
              (message-questions message)))))



;; ----------------------------------

(defun get-host-by-name (name)
  "Resolve a hostname into a list of SOCKADDR-IN addresses.
NAME ::= string hostname.
Returns a list of FSOCKET:SOCKADDR-IN addresses."
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

