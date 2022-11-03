(in-package :threadmill)

(defconstant +metadata-entries-per-group+ 16
  "The number of metadata entries we store per group.")

(deftype metadata-group ()
  `(sb-ext:simd-pack (unsigned-byte 8)))
(deftype metadata-vector ()
  `(simple-array (unsigned-byte 8) 1))
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
  (sse2:u8.16-movemask
   (sse2:u8.16= group (%sse2-broadcast-byte byte))))

(defun writable (group)
  "Return matches for metadata bytes we can put new mappings in."
  ;; movemask tests the high bit of each byte, and we want to test the
  ;; high bit, so we have nothing else to do. Magic!
  (sse2:u8.16-movemask group))

(defun match-union (m1 m2)
  (logior m1 m2))

(declaim (inline call-with-matches matches-p))
(defun call-with-matches (bit-mask continuation)
  (declare (function continuation)
           (optimize (speed 3) (safety 0))
           ((unsigned-byte 16) bit-mask))
  (loop until (zerop bit-mask)
        do (funcall continuation (bsf/16 bit-mask))
           (setf bit-mask (logand bit-mask (1- bit-mask)))))

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
  (let ((vector (make-array size
                            :element-type '(unsigned-byte 8)
                            :initial-element +empty-metadata+)))
    vector))

(declaim (inline metadata-group metadata-groups
                 cas-metadata metadata (setf metadata)))

(defun metadata-group (vector position)
  "Retrieve the Nth metadata group from a vector. 
Note that N has a length of an element."
  (declare (metadata-vector vector)
           (vector-index position)
           (optimize (speed 3) (safety 0)))
  (sse2:u8.16-aref vector position))

(defun metadata-groups (metadata)
  (floor (length metadata) +metadata-entries-per-group+))

(defun metadata (vector position)
  (declare (vector-index position))
  (aref vector position))

(defun cas-metadata (vector position old-value new-value)
  (= old-value
     (cas-byte vector position old-value new-value)))

(defun (setf metadata) (new-value vector position)
  (declare (vector-index position))
  (setf (aref vector position) new-value))

(defmacro atomic-setf (place value)
  `(prog1
       (setf ,place ,value)
     (sb-vm:%memory-barrier)))
