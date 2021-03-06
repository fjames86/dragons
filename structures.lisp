;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file defines the various structures and their binary encodings.

(in-package #:dragons)

;; TODO: we need to support compression when encoding
;; if we have a hope of writing a functioning name server

;; -------------------------

(defun encode-uint16 (uint16 blk)
  (setf (nibbles:ub16ref/be (xdr-block-buffer blk)
			    (xdr-block-offset blk))
	uint16)
  (incf (xdr-block-offset blk) 2))
(defun decode-uint16 (blk)
  (let ((u (nibbles:ub16ref/be (xdr-block-buffer blk)
			       (xdr-block-offset blk))))
    (incf (xdr-block-offset blk) 2)
    u))

(defun encode-int32 (int32 blk)
  (setf (nibbles:sb32ref/be (xdr-block-buffer blk)
			    (xdr-block-offset blk))
	int32)
  (incf (xdr-block-offset blk) 4))
(defun decode-int32 (blk)
  (let ((u (nibbles:sb32ref/be (xdr-block-buffer blk)
			       (xdr-block-offset blk))))
    (incf (xdr-block-offset blk) 4)
    u))

(defun encode-string (string blk)
  (let ((octets (babel:string-to-octets string)))
    (setf (aref (xdr-block-buffer blk) (xdr-block-offset blk))
	  (length octets))
    (incf (xdr-block-offset blk))
    (dotimes (i (length octets))
      (setf (aref (xdr-block-buffer blk) (+ (xdr-block-offset blk) i))
	    (aref octets i)))
    (incf (xdr-block-offset blk) (length octets))))

(defun decode-string (blk)
  (let ((len (aref (xdr-block-buffer blk) (xdr-block-offset blk))))
    (incf (xdr-block-offset blk))
    (prog1
	(babel:octets-to-string (xdr-block-buffer blk)
				:start (xdr-block-offset blk)
				:end (+ (xdr-block-offset blk) len))
      (incf (xdr-block-offset blk) len))))

;; -------------------------

(defun encode-label (string blk)
  (encode-string string blk))

(defun decode-label-pointer (blk)
  "Same as declde-label but forbid resolving pointers"
  (let ((len (aref (xdr-block-buffer blk) (xdr-block-offset blk))))
    (incf (xdr-block-offset blk))
    (cond
      ((= len 0) (values nil nil))
      ((<= len 63)
       (let ((str (babel:octets-to-string (xdr-block-buffer blk)
					  :start (xdr-block-offset blk)
					  :end (+ (xdr-block-offset blk) len))))
	 (incf (xdr-block-offset blk) len)
         (values str nil)))
      (t (error "Attempt to resolve pointer within pointer")))))

(defun decode-label (blk rdepth)
  (let ((len (aref (xdr-block-buffer blk) (xdr-block-offset blk))))
    (incf (xdr-block-offset blk))
    (cond
      ((= len 0) (values nil nil))
      ((<= len 63)
       (let ((str (babel:octets-to-string (xdr-block-buffer blk)
					  :start (xdr-block-offset blk)
					  :end (+ (xdr-block-offset blk) len))))
	 (incf (xdr-block-offset blk) len)
         (values str nil)))
      ((= (logand len #b11000000) #b11000000)
       ;; this is a pointer
       (let* ((offset (logior (ash (logand len #b00111111) 8)
			      (prog1 (aref (xdr-block-buffer blk) (xdr-block-offset blk))
				(incf (xdr-block-offset blk)))))
	      (v (make-xdr-block :buffer (xdr-block-buffer blk)
				 :offset offset
				 :count (xdr-block-count blk))))
	 ;; FIXME: this is potentially recursive if someone sent us a malformed message.
	 ;; We cannot forbid resolving pointers when already resolving pointers
	 ;; because it seems some servers return such messages.
	 ;; TODO: prevent infinitely recursive pointer resolution. We could
	 ;; keep a list of pointers we have resolved so far, or pass a recursion depth
	 ;; as a parameter.
	 (when (>= rdepth 10) (error "Maximum pointer recursion depth"))
	 (values (decode-name v (1+ rdepth)) t))) ;;(decode-name-pointer v) t)))
      (t (error "Invalid pointer header #b~B" (logand len #b11000000))))))


;; -------------------------

;;(defvar *pointer-offsets* nil "List of (name offset) forms recording locations of previously encoded names.")

(defun encode-name (name blk)
  "Encode a dotted string name as a list of labels to the stream."
  (let ((*pointer-offsets* nil))
    (declare (special *pointer-offsets*))
    (do ((pos 0))
	((>= pos (length name))
	 (encode-label "" blk))
      ;; first we check to see whether we have already written this portion of the
      ;; name, if so write a pointer and exit
      (let ((offset (cadr (find (subseq name pos) *pointer-offsets*
				:key #'car :test #'string=))))
	(when offset
	  (encode-uint16 (logior (ash #b11000000 8) offset) blk)
	  (return-from encode-name nil)))
      
      ;; we are writing ourselves in, record our position
      (push (list (subseq name pos) (xdr-block-offset blk))
	    *pointer-offsets*)
      
      ;; write the name 
      (let ((p (position #\. name :test #'char= :start pos)))
	(cond
	  (p
	   (encode-label (subseq name pos p) blk)
	   (setf pos (1+ p)))
	  (t 
	   (encode-label (subseq name pos) blk)
	   (setf pos (length name))))))))

(defun decode-name-pointer (blk)
  "Decode a list of labels from the stream. Returns a dotted string."
  (with-output-to-string (s)
    (do ((done nil)
	 (first t))
	(done)
      (let ((label (decode-label-pointer blk)))
	(cond
	  ((null label) (setf done t))
	  ((string= label "")
	   (setf done t))
	  (t 
	   (unless first (write-char #\. s))
	   (setf first nil)
	   (write-string label s)))))))
  
(defun decode-name (blk &optional (rdepth 0))
  "Decode a list of labels from the stream. Returns a dotted string."
  (with-output-to-string (s)
    (do ((done nil)
	 (first t))
	(done)
      (multiple-value-bind (label pointer-p) (decode-label blk rdepth)
	(when pointer-p (setf done t))
	(cond
	  ((null label) (setf done t))
	  ((string= label "")
	   (setf done t))
	  (t 
	   (unless first (write-char #\. s))
	   (setf first nil)
	   (write-string label s)))))))

;; -------------------------


(defparameter *type-codes*
  '((:a 1)
    (:ns 2)
    (:cname 5)
    (:soa 6)
    (:mb 7)
    (:mg 8)
    (:mr 9)
    (:null 10)
    (:wks 11)
    (:ptr 12)
    (:hinfo 13)
    (:minfo 14)
    (:mx 15)
    (:txt 16)
    (:aaaa 28)
    (:srv 33)
    (:axfr 252)
    (:mailb 253)
    (:maila 254)
    (:all 255)
    (:caa 257)))

(defparameter *class-codes*
  '((:in 1) ;; internet 
    (:ch 3) ;; CHAOS
    (:hs 4))) ;; HESIOD ??

;; ----------------------------

(defgeneric encode-rdata (type data blk)
  (:documentation "Encode a object of specified type to the stream. TYPE should be a keyword in *TYPE-CODES*."))
(defgeneric decode-rdata (type blk)
  (:documentation "Decode an object of specified type from the stream. The stream contains all the data for the object,
so you may read until EOF to extract all the information."))

;; default methods leave data untouched and assume the data is just a vector
(defmethod encode-rdata (type data blk)
  (dotimes (i (length data))
    (setf (aref (xdr-block-buffer blk) (+ (xdr-block-offset blk) i))
	  (aref data i)))
  (incf (xdr-block-offset blk) (length data)))
(defmethod decode-rdata (type blk)
  (subseq (xdr-block-buffer blk)
	  (xdr-block-offset blk)
	  (xdr-block-count blk)))

;; ------- cname -----

(defmethod encode-rdata ((type (eql :cname)) data blk)
  (encode-name data blk))

(defmethod decode-rdata ((type (eql :cname)) blk)
  (decode-name blk))

;; -------- hinfo -----

(defmethod encode-rdata ((type (eql :hinfo)) data blk)
  (let ((cpu (getf data :cpu))
        (os (getf data :os)))
    (encode-string cpu blk)
    (encode-string os blk)))

(defmethod decode-rdata ((type (eql :hinfo)) blk)
  (let ((cpu (decode-string blk))
        (os (decode-string blk)))
    (list :cpu cpu :os os)))

;; ----------- madname --------

(defmethod encode-rdata ((type (eql :mb)) data blk)
  (encode-name data blk))

(defmethod decode-rdata ((type (eql :mb)) blk)
  (decode-name blk))

;; ---------- a -------------

;; address.
(defmethod encode-rdata ((type (eql :a)) data blk)
  (let ((inaddr (etypecase data
		  (string (fsocket::dotted-quad-to-inaddr data))
		  (vector data))))
    (assert (= (length inaddr) 4))
    (dotimes (i 4)
      (setf (aref (xdr-block-buffer blk) (+ (xdr-block-offset blk) i))
	    (aref inaddr i)))
    (incf (xdr-block-offset blk) 4)))

;; don't provide a method for decoding Address types because it's just a vector which is the same as the default method

;; ----------- aaaa -----------------

(defmethod encode-rdata ((type (eql :aaaa)) data blk)
  (let ((buffer (xdr-block-buffer blk))
	(offset (xdr-block-offset blk)))
    (dotimes (i 8)
      (let ((u16 (aref data i)))
	(setf (aref buffer (+ offset (* 2 i)))
	      (logand (ash u16 -8) #xff)
	      (aref buffer (+ offset 1 (* 2 i)))
	      (logand u16 #xff))))
    (incf (xdr-block-offset blk) 16)))

(defmethod decode-rdata ((type (eql :aaaa)) blk)
  (let ((inaddr6 (make-array 8))
	(buffer (xdr-block-buffer blk))
	(offset (xdr-block-offset blk)))
    (dotimes (i 8)
      (setf (aref inaddr6 i)
	    (logior (ash (aref buffer (+ offset (* i 2))) 8)
		    (aref buffer (+ offset 1 (* i 2))))))
    (incf (xdr-block-offset blk) 16)
    inaddr6))

;; ----------- ns -----------------

(defmethod encode-rdata ((type (eql :ns)) data blk)
  (encode-name data blk))

(defmethod decode-rdata ((type (eql :ns)) blk)
  (decode-name blk))

;; ----------- txt ----------------

(defmethod encode-rdata ((type (eql :txt)) data blk)
  (encode-string data blk))

(defmethod decode-rdata ((type (eql :txt)) blk)
  (decode-string blk))

;; ------------- srv ----------------

(defmethod encode-rdata ((type (eql :srv)) data blk)
  (let ((priority (getf data :priority))
        (weight (getf data :weight))
        (port (getf data :port))
        (target (getf data :target))
	(buffer (xdr-block-buffer blk))
	(offset (xdr-block-offset blk)))
    (setf (nibbles:ub16ref/be buffer offset) priority
	  offset (+ offset 2)
	  (nibbles:ub16ref/be buffer offset) weight
	  offset (+ offset 2)
	  (nibbles:ub16ref/be buffer offset) port
	  offset (+ offset 2))
    (setf (xdr-block-offset blk) offset)
    (encode-name target blk)))

(defmethod decode-rdata ((type (eql :srv)) blk)
  (let ((buffer (xdr-block-buffer blk))
	(offset (xdr-block-offset blk))
	priority weight port target)		 
    (setf priority (nibbles:ub16ref/be buffer offset)
	  offset (+ offset 2)
	  weight (nibbles:ub16ref/be buffer offset)
	  offset (+ offset 2)
	  port (nibbles:ub16ref/be buffer offset)
	  offset (+ offset 2))
    (setf (xdr-block-offset blk) offset)
    (setf target (decode-name blk))
    (list :priority priority
          :weight weight
          :port port
          :target target)))

;; --------------------- ptr ------------------

(defmethod encode-rdata ((type (eql :ptr)) data blk)
  (encode-name data blk))

(defmethod decode-rdata ((type (eql :ptr)) blk)
  (decode-name blk))

;; ------------------- wks ------------------
;; section 3.4.2

;; TODO: the bitmap is a bitmap of available ports. So if bit 26 is set,
;; it means a service on port 25 is available. We should operate on a
;; list of ports rather than the raw bitmap itself.
;; (defmethod encode-rdata ((type (eql :wks)) data blk)
;;   (destructuring-bind (&key addr protocol bmap) data
;;     ;; address (4 octet inaddr)
;;     (let ((inaddr (etypecase data
;; 		    (string (fsocket::dotted-quad-to-inaddr data))
;; 		    (vector data))))
;;       (dotimes (i (length inaddr))
;; 	(setf (aref (xdr-block-buffer blk) (+ (xdr-block-offset blk) i))
;; 	      (aref inaddr i)))
;;       (incf (xdr-block-offset blk) (length inaddr)))
;;     ;; protocol (8-bit IP protocol)
;;     (let ((p (cond
;; 	       ((eq protocol :udp) fsocket::+ipproto-udp+)
;; 	       ((eq protocol :tcp) fsocket::+ipproto-tcp+)
;; 	       ((not (typep protocol 'integer)) (error "Protocol must be an integer"))
;; 	       (t protocol))))
;;       (setf (aref (xdr-block-buffer blk) (xdr-block-offset blk)) p)
;;       (incf (xdr-block-offset blk)))
;;     ;; bit map (octet vector)
;;     (dotimes (i (length bmap))
;;       (setf (aref (xdr-block-buffer blk) (+ (xdr-block-offset blk) i))
;; 	    (aref bmap i)))
;;     (incf (xdr-block-offset blk) (length bmap))))

;; (defmethod decode-rdata ((type (eql :wks)) blk)
;;   (let (inaddr protocol bmap)
;;     (setf inaddr (subseq (xdr-block-buffer blk)
;; 			 (xdr-block-offset blk)
;; 			 (+ (xdr-block-offset blk) 4)))
;;     (incf (xdr-block-offset blk) 4)
;;     (let ((p (aref (xdr-block-buffer blk) (xdr-block-offset blk))))
;;       (setf protocol
;; 	    (cond
;; 	      ((= p fsocket::+ipproto-udp+) :udp)
;; 	      ((= p fsocket::+ipproto-tcp+) :tcp)
;; 	      (t p)))
;;       (incf (xdr-block-offset blk)))
;;     (setf bmap
;; 	  (subseq (xdr-block-buffer blk)
;; 		  (xdr-block-offset blk)
;; 		  (xdr-block-count blk)))
;;     (setf (xdr-block-offset blk) (xdr-block-count blk))
;;     (list :inaddr inaddr :protocol protocol :bmap bmap)))
				    
;; ------------------- mx  ------------------ 

;; Thanks for Mike Maul for the addition of MX records. 23/2/2016.
(defmethod encode-rdata ((type (eql :mx)) data blk)
  (encode-uint16 (getf data :preference) blk)
  (encode-name (getf data :exchange) blk))

(defmethod decode-rdata ((type (eql :mx)) blk)
  (list :preference (decode-uint16 blk)
	:exchange (decode-name blk)))

;; -------------------------------------------

;; see rfc1035 section 3.3.13

(defmethod encode-rdata ((type (eql :soa)) data blk)
  (encode-name (getf data :mname) blk)
  (encode-name (getf data :rname) blk)
  (encode-uint32 blk (getf data :serial))
  (encode-uint32 blk (getf data :refresh))
  (encode-uint32 blk (getf data :retry))
  (encode-uint32 blk (getf data :expire))
  (encode-uint32 blk (getf data :minimum)))

(defmethod decode-rdata ((type (eql :soa)) blk)
  (list :mname (decode-name blk)
        :rname (decode-name blk)
        :serial (decode-uint32 blk)
        :refresh (decode-uint32 blk)
        :retry (decode-uint32 blk)
        :expire (decode-uint32 blk)
        :minimum (decode-uint32 blk)))


;; --------------------------------------------

;; resource record structure 
(defstruct rr 
  name type class ttl rdata)

(defun encode-rr (blk rr)
  (let ((name (rr-name rr))
        (type (rr-type rr))
        (class (rr-class rr))
        (ttl (rr-ttl rr))
        (rdata (rr-rdata rr)))
    ;; name
    (encode-name name blk)
    ;; type
    (encode-uint16 (or (cadr (assoc type *type-codes*))
		       (error "Unknown TYPE ~A" type))
		   blk)
    ;; class
    (encode-uint16 (cadr (assoc class *class-codes*))
		   blk)    
    ;; ttl
    (encode-int32 ttl blk)

    ;; we save 2 octets as space for the length, but we don't know the length until
    ;; after the generic function has encoded it, so we go back after and write the length in
    (let ((offset (xdr-block-offset blk)))
      (incf (xdr-block-offset blk) 2)
      (encode-rdata type rdata blk)
      (setf (nibbles:ub16ref/be (xdr-block-buffer blk) offset)
	    (- (xdr-block-offset blk) offset 2)))))

(defun decode-rr (blk)
  (let (name type class ttl len)
    (setf name (decode-name blk))
    (setf type (decode-uint16 blk))
    (setf class (decode-uint16 blk))
    (setf ttl (decode-int32 blk))
    (setf len (decode-uint16 blk))
    (let* ((tname (car (find type *type-codes*
			     :key #'cadr :test #'=)))
	   (rblk (make-xdr-block :buffer (xdr-block-buffer blk)
				 :offset (xdr-block-offset blk)
				 :count (+ (xdr-block-offset blk) len)))
	   (rdata (decode-rdata tname rblk)))
      (unless tname (warn "Unknown RR type ~A" type))
      (incf (xdr-block-offset blk) len)
      (make-rr :name name
	       :type tname
	       :class (car (find class *class-codes*
				 :key #'cadr :test #'=))
	       :ttl ttl
	       :rdata rdata))))

;;; -----------------------------


(defstruct header 
  id qr opcode rcode
  qcount acount ncount rcount
  aa-p trunc-p rd-p ra-p)

(defconstant +flag-qr+ #x8000)
(defconstant +flag-aa+ #x0400)
(defconstant +flag-tc+ #x0200)
(defconstant +flag-rd+ #x0100)
(defconstant +flag-ra+ #x0080)

;; HEADER format: |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
(defun encode-header (blk header)
  (encode-uint16 (header-id header) blk)
  (encode-uint16 (logior (ecase (header-qr header)
                                   (:query 0)
                                   (:reply (ash 1 15)))
			 (ash (ecase (header-opcode header)
				(:query 0)
                                        (:iquery 1)
                                        (:status 2))
			      11)
			 (if (header-aa-p header) +flag-aa+ 0)
			 (if (header-trunc-p header) +flag-tc+ 0)
			 (if (header-rd-p header) +flag-rd+ 0)
			 (if (header-ra-p header) +flag-ra+ 0)
			 (ecase (header-rcode header)
			   (:ok 0)
			   (:format-error 1)
			   (:server-failure 2)
			   (:name-error 3)
			   (:not-implemented 4)
			   (:refused 5)))
		 blk)
  (encode-uint16 (header-qcount header) blk)
  (encode-uint16 (header-acount header) blk) 
  (encode-uint16 (header-ncount header) blk)
  (encode-uint16 (header-rcount header) blk))


(defun decode-header (blk)
  (let ((h (make-header)))
    (setf (header-id h) (decode-uint16 blk))
    (let ((flags (decode-uint16 blk)))
      (if (logtest flags +flag-qr+)
          (setf (header-qr h) :reply)
          (setf (header-qr h) :query))
      (setf (header-aa-p h) (logtest flags +flag-aa+)
	    (header-trunc-p h) (logtest flags +flag-tc+)
	    (header-rd-p h) (logtest flags +flag-rd+)
	    (header-ra-p h) (logtest flags +flag-ra+))
      (setf (header-opcode h)
            (ecase (logand (ash flags -11) #x3)
              (0 :query)
              (1 :iquery)
              (2 :status)))
      (setf (header-rcode h)
            (ecase (logand flags #x7)
              (0 :ok)
              (1 :format-error)
              (2 :server-failure)
              (3 :name-error)
              (4 :not-implemented)
              (5 :refused))))
    (setf (header-qcount h) (decode-uint16 blk)
          (header-acount h) (decode-uint16 blk)
          (header-ncount h) (decode-uint16 blk)
          (header-rcount h) (decode-uint16 blk))
    h))


;; questions

(defun encode-question (blk question)
  (let ((name (getf question :name))
        (type (getf question :type))
        (class (getf question :class)))
    (encode-name name blk)
    (encode-uint16 (or (cadr (assoc type *type-codes*))
		       (error "Unknown type code ~A, expected one of ~S" 
			      type (mapcar #'car *type-codes*)))
		   blk)
    (encode-uint16 (or (cadr (assoc class *class-codes*))
		       (error "Unknown class code ~A, expected one of ~S"
			      class (mapcar #'car *class-codes*)))
		   blk)))

(defun decode-question (blk)  
  (let ((name (decode-name blk))
        (type (decode-uint16 blk))
        (class (decode-uint16 blk)))
    (unless (find type *type-codes* :key #'cadr :test #'=)
      (warn "Unknown TYPE ~S" type))
    (list :name name 
          :type (car (find type *type-codes* :key #'cadr :test #'=))
          :class (car (find class *class-codes* :key #'cadr :test #'=)))))

(defun question (name &optional (type :a) (class :in))
  "Allocate a question to use with QUERY.

NAME ::= a hostname.
TYPE ::= the type of query.
CLASS ::= the class of query, almost always :IN (internet).
"
  (declare (type string name)
           (type symbol type class))
  (list :name name :type type :class class))

;; --------------------

;; message
;; TODO: support compression when encoding

(defstruct message 
  header questions answers authorities additionals)

(defun encode-message (blk message)
  (let ((*pointer-offsets* nil))
    (declare (special *pointer-offsets*))
    (encode-header blk (message-header message))
    (dolist (q (message-questions message))
      (encode-question blk q))
    (dolist (answer (message-answers message))
      (encode-rr blk answer))
    (dolist (auth (message-authorities message))
      (encode-rr blk auth))
    (dolist (a (message-additionals message))
      (encode-rr blk a))))
  
(defun decode-message (blk)
  (let* ((header (decode-header blk))
	 (msg (make-message :header header)))
    (setf (message-questions msg)
	  (loop :for i :below (header-qcount header)
	     :collect (decode-question blk))
	  (message-answers msg)
	  (loop :for i :below (header-acount header)
	     :collect (decode-rr blk))
	  (message-authorities msg)
	  (loop :for i :below (header-ncount header)
	     :collect (decode-rr blk))
	  (message-additionals msg)
	  (loop :for i :below (header-rcount header)
	     :collect (decode-rr blk)))
    msg))

(defun message (questions &key (qr :query) (opcode :query) (rcode :ok) answers authorities additionals id (rd-p t) ra-p trunc-p aa-p)
  "Make a DNS message. 
QUESTIONS ::= a list of questions as returned by QUESTION
QD ::= either :query or :reply 
OPCODE ::= a symbol naming the mode of operation. Either :query, :iquery or :status
RCODE ::= result code 
ANSWERS. AUTHORITIES, ADDITIONALS ::= a list of RR structures 
ID ::= integer transaction ID
AA-P ::= authoritative answer. for replies, indicates the responding server
is the authoritative server for the domain name in question.
RD-P ::= for clients, indicates that a recurisive answer is desired. This flag
should be copied into responses.
RA-P: ::= indicates that the server supports recursion.
TRUNC-P ::= indicates the message was truncated.
"
  (make-message :header (make-header :opcode opcode
                                     :id (or id (random #xffff))
                                     :qr qr
                                     :rcode rcode
                                     :qcount (length questions)
                                     :acount (length answers)
                                     :ncount (length authorities)
                                     :rcount (length additionals)
				     :aa-p aa-p
				     :rd-p rd-p
				     :ra-p ra-p
				     :trunc-p trunc-p)
                :questions questions
                :answers answers
                :authorities authorities
                :additionals additionals))

