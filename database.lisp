;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:dragons)

;; this file defines a database for caching resource records.
;; Can also be used as the source of a DNS server.

(defstruct db 
  mapping stream count)

(defvar *db* nil)
(defvar *database-path* (merge-pathnames "dragons.dat" (user-homedir-pathname)))

;; FIXME: this is not guaranteed to be large enough to hold a record. 
;; names can be up to 255 characters, the record itself larger still.
;; Reserving e.g. 1k per entry seems excessive (or is it?)
;; Otherwise we'd need to be able to store variable length records, which is a pain.
(defconstant +block-size+ 1024)

;; in case the binary formats change we can detect incompatibilities
(defconstant +db-version+ 1)

(defun read-db-header ()
  (let ((stream (db-stream *db*)))
    (file-position stream 0)
    (list :count (nibbles:read-ub32/be stream)
	  :seqno (nibbles:read-ub32/be stream)
	  :version (nibbles:read-ub32/be stream))))

(defun read-db-count ()
  (let ((header (read-db-header)))
    (getf header :count)))

(defun inc-db-seqno ()
  (file-position (db-stream *db*) 4)
  (let ((seqno (nibbles:read-ub32/be (db-stream *db*))))
    (nibbles:write-ub32/be (1+ seqno) (db-stream *db*))))

(defun close-db ()
  (when *db*
    (pounds:close-mapping (db-mapping *db*))
    (setf *db* nil)))

(defun open-db (&optional (count 32))
  (cond
    (*db*
     ;; already open, check the real count matches the count we think
     (let ((real-count (read-db-count)))
       (unless (= real-count (db-count *db*))
	 ;; count doesn't match, we must remap
	 (close-db)
	 (open-db real-count))))
    (t 
     ;; open it
     (let ((mapping (pounds:open-mapping *database-path* 
					 (* count +block-size+))))
       (setf *db* (make-db :mapping mapping
			   :stream (pounds:make-mapping-stream mapping)
			   :count count))
       (let* ((header (read-db-header))
	      (real-count (getf header :count)))
	 (cond
	   ((zerop real-count)
	    ;; newly created file, write the initial header
	    (file-position (db-stream *db*) 0)
	    (nibbles:write-ub32/be count (db-stream *db*))
	    (nibbles:write-ub32/be 1 (db-stream *db*))
	    (nibbles:write-ub32/be +db-version+ (db-stream *db*)))
	   ((not (= +db-version+ (getf header :version)))
	    (close-db)
	    (error "Database version ~A mismatch with ~A." (getf header :version) +db-version+))
	   ((= count real-count) 
	    count)
	   ((< count real-count)
	    ;; need to remap to the real count
	    (close-db)
	    (open-db real-count))
	   ((> count real-count)
	    ;; write the new count
	    (file-position (db-stream *db*) 0)
	    (nibbles:write-ub32/be count (db-stream *db*)))))))))


;; each record is:
;; (active boolean)
;; (expiry uint64)
;; rr

(defun encode-record (stream rr expiration)
  (nibbles:write-ub32/be 1 stream)
  (nibbles:write-ub64/be expiration stream)
  (encode-rr stream rr))

(defun decode-record (stream)
  (let ((active (not (zerop (nibbles:read-ub32/be stream)))))
    (when active 
      (let ((expiry (nibbles:read-ub64/be stream))
	    (rr (decode-rr stream)))
	(values rr expiry)))))

(defun default-test (rr)
  (declare (type rr rr))
  (typecase (rr-rdata rr)
    (string #'string-equal)
    (vector #'equalp)
    (otherwise #'eql)))


(defun add-record (rr &optional expiration test)
  (declare (type rr rr))
  (open-db)
  (pounds:with-locked-mapping ((db-stream *db*))
    ;; iterate over the records
    (do ((i 1 (1+ i))
	 (count (db-count *db*))
	 (stream (db-stream *db*))
	 (now (get-universal-time)))
	((= i count))
      (file-position stream (* i +block-size+))
      (multiple-value-bind (entry expiry) (decode-record stream)
	(cond
	  ((and entry (< expiry now))
	   ;; active but expired, delete the entry 
	   (file-position stream (* i +block-size+))
	   (nibbles:write-ub32/be 0 stream))
	  (entry
	   ;; there is an entry, check if it is equal
	   (when (and (eq (rr-type rr) (rr-type entry))
		      (eq (rr-class rr) (rr-class entry))
		      (string-equal (rr-name rr) (rr-name entry))
		      (if (rr-rdata rr)
			  (funcall (or test (default-test rr))
				   (rr-rdata rr) (rr-rdata entry))
			  t))
	     ;; entry already there
	     (file-position stream (* i +block-size+))
	     (encode-record stream rr (or expiration #xffffffffffffffff))
	     ;; increment the seqno
	     (inc-db-seqno)
	     (return-from add-record rr)))
	  (t 
	   ;; this record is inactive, write the new one
	   (file-position stream (* i +block-size+))
	   (encode-record stream rr (or expiration #xffffffffffffffff))
	   (return-from add-record rr))))))
  ;; not there, no spare entries. need to remap and grow
  (let ((c (db-count *db*)))
    (close-db)
    (open-db (* c 2))
    ;; add new entry 
    (pounds:with-locked-mapping ((db-stream *db*))
      (file-position (db-stream *db*) (* c +block-size+))
      (encode-record (db-stream *db*) rr #xffffffffffffffff)
      ;; increment the seqno
      (inc-db-seqno))
    rr))

(defun insert-record (name rdata &optional (type :a) (ttl 300) (class :in))
  "Insert a static record into the database."
  (add-record (make-rr :name name
		       :type type
		       :class class
		       :ttl ttl
		       :rdata rdata)
	      #xffffffffffffffff))

(defun find-record (rr &optional test)
  (declare (type rr rr))
  (open-db)
  (pounds:with-locked-mapping ((db-stream *db*))
    ;; iterate over the records
    (do ((i 1 (1+ i))
	 (count (db-count *db*))
	 (stream (db-stream *db*))
	 (now (get-universal-time)))
	((= i count))
      (file-position stream (* i +block-size+))
      (multiple-value-bind (entry expiry) (decode-record stream)
	(cond
	  ((and entry (< expiry now))
	   ;; active but expired, delete the entry 
	   (file-position stream (* i +block-size+))
	   (nibbles:write-ub32/be 0 stream)
	   nil)
	  (entry
	   ;; there is an entry, check if it is eql
	   (when (and (eq (rr-type rr) (rr-type entry))
		      (eq (rr-class rr) (rr-class entry))
		      (string-equal (rr-name rr) (rr-name entry))
		      (if (rr-rdata rr)
			  (funcall (or test (default-test rr))
				   (rr-rdata rr) (rr-rdata entry))
			  t))
	     ;; entry found
	     (return-from find-record entry))))))))

(defun remove-record (name rdata &optional (type :a) (ttl 300) (class :in))
  "Remove the record from the database."
  (let ((rr (make-rr :name name
		     :type type
		     :class class
		     :ttl ttl
		     :rdata rdata)))
    (declare (type rr rr))
    (open-db)
    (pounds:with-locked-mapping ((db-stream *db*))
      ;; iterate over the records
      (do ((i 1 (1+ i))
	   (count (db-count *db*))
	   (stream (db-stream *db*))
	   (now (get-universal-time)))
	  ((= i count))
	(file-position stream (* i +block-size+))
	(multiple-value-bind (entry expiry) (decode-record stream)
	  (cond
	    ((and entry (< expiry now))
	     ;; active but expired, delete the entry 
	     (file-position stream (* i +block-size+))
	     (nibbles:write-ub32/be 0 stream)
	     (inc-db-seqno)
	     nil)
	    (entry
	     ;; there is an entry, check if it is eql
	     (when (and (eq (rr-type rr) (rr-type entry))
			(eq (rr-class rr) (rr-class entry))
			(string-equal (rr-name rr) (rr-name entry))
			(funcall (default-test rr) 
				 (rr-rdata rr) (rr-rdata entry)))
	       ;; entry found, delete it
	       (file-position stream (* i +block-size+))
	       (nibbles:write-ub32/be 0 stream)
	       (inc-db-seqno)
	       (return-from remove-record)))))))))


(defun list-records ()  
  (open-db)
  (pounds:with-locked-mapping ((db-stream *db*))
    ;; iterate over the records
    (do ((i 1 (1+ i))
	 (count (db-count *db*))
	 (stream (db-stream *db*))
	 (now (get-universal-time))
	 (records nil))
	((= i count) records)
      (file-position stream (* i +block-size+))
      (multiple-value-bind (entry expiry) (decode-record stream)
	(cond
	  ((and entry (< expiry now))
	   ;; active but expired, delete the entry 
	   (file-position stream (* i +block-size+))
	   (nibbles:write-ub32/be 0 stream)
	   nil)
	  (entry
	   (push entry records)))))))

