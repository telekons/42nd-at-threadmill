(in-package :threadmill)

(defconstant +metadata-entries-per-group+ 16
  "The number of metadata entries we store per group.")

(deftype metadata-group ()
  `(sse:sse-pack (unsigned-byte 8)))
(deftype metadata-vector ()
  `(sse:sse-array (unsigned-byte 8) 1))
(deftype metadata-index ()
  `(mod ,(floor most-positive-fixnum +metadata-entries-per-group+)))
(deftype vector-index ()
  `(and fixnum unsigned-byte))

(defconstant +empty-metadata+ #x80
  "The metadata byte stored for an empty entry.")

(declaim (inline bytes matches-p writable mask-h2 match-union))

(defun mask-h2 (h2)
  "Mask off part of the H2 hash, for use as metadata."
  (declare ((unsigned-byte 8) h2))
  (logand #x7f h2))

(defun bytes (byte group)
  "Return matches for a byte in a metadata group."
  (declare ((unsigned-byte 8) byte))
  (sse:movemask-pi8
   (sse:=-pi8 (sse:set1-pi8 byte) group)))

(defun writable (group)
  "Return matches for metadata bytes we can put new mappings in."
  (bytes +empty-metadata+ group))

(defun match-union (m1 m2)
  (logior m1 m2))

(declaim (inline call-with-matches))
(defun call-with-matches (bit-mask continuation)
  (declare (function continuation)
           ((unsigned-byte 16) bit-mask)
           (optimize (speed 3) (safety 0)))
  (when (zerop bit-mask)
    (return-from call-with-matches))
  (let ((position 0))
    (declare (fixnum position))
    ;; We have decided that the bit-mask is non-zero, so there must
    ;; be a 1 in it. First, we find the first 1.
    (setf position (bsf/16 bit-mask)
          bit-mask (ash bit-mask (- position)))
    (loop
      ;; Call the continuation with this position.
      (funcall continuation position)
      ;; We're done when the bit-mask is 1, i.e. there are no more 1 bits in
      ;; this mask.
      (when (= 1 bit-mask)
        (return-from call-with-matches))
      ;; Find the next 1.
      (let ((next-jump (bsf/16 (logand bit-mask #xFFFE))))
        (setf bit-mask (ash bit-mask (- next-jump))
              position (ldb (byte 62 0) (+ position next-jump)))))))

(defmacro do-matches ((position bit-mask) &body body)
  "Evaluate BODY with POSITION bound to every match in the provided BIT-MASK."
  (let ((continuation (gensym "CONTINUATION")))
    `(flet ((,continuation (,position)
              (declare ((mod ,+metadata-entries-per-group+) ,position))
              ,@body))
       (declare (inline ,continuation)
                (dynamic-extent #',continuation))
       (call-with-matches ,bit-mask #',continuation))))

(defun matches-p (bit-mask)
  "Are there any matches in BIT-MASK?"
  (plusp bit-mask))

(defun make-metadata-vector (size)
  "Create a metadata vector for a hash table of a given size, with all elements initialized to +EMPTY-METADATA+."
  (let ((vector (sse:make-sse-array size
                                    :element-type '(unsigned-byte 8)
                                    :initial-element +empty-metadata+)))
    vector))

(declaim (inline metadata-group metadata-groups
                 cas-metadata metadata (setf metadata)))

(defun metadata-group (vector position)
  "Retrieve the Nth metadata group from a vector. 
Note that N has a length of a group; on a 8-element-per-group implementation, 
(metadata-group V 1) retrieves the 8th through 15th metadata bytes of V."
  (declare (metadata-vector vector)
           (metadata-index position)
           (optimize (speed 3) (safety 0)))
  ;; Why won't SSE:AREF-PI work?
  (sse:mem-ref-pi (sb-sys:vector-sap vector)
                  (* position +metadata-entries-per-group+)))

(defun metadata-groups (metadata)
  (floor (length metadata) +metadata-entries-per-group+))

;; Will we CAS or unconditionally set metadata? As I understand it, whichever
;; thread wins the key CAS gets to set the metadata, so it can't change by another
;; thread.
#+(or)
(defun cas-metadata (vector position old new)
  (= old
     (cas-byte vector position old new)))

(defun metadata (vector position)
  (declare (vector-index position))
  (aref vector position))

(defun (setf metadata) (new-value vector position)
  (declare (vector-index position))
  (setf (aref vector position)  new-value))

(defmacro atomic-setf (place value)
  `(prog1
       (setf ,place ,value)
     (sb-vm:%memory-barrier)))
