(in-package :threadmill)

;;; The "storage vector" for a hash table contains four slots before the
;;; keys and values:
;;; 1. The metadata vector
;;; 2. The new storage vector we're copying to (or NIL)
;;; 3. How many elements we have finished copying (used when resizing)
;;; 4. How many elements we are going to copy (used when resizing)
;;; Then the rest of the table just contains a key, then a value, and so on.

;;; We define macros so that we can SETF and CAS the position, without
;;; having to write setters and (unportable) CAS-ers.
(defconstant +words-before-values+ 8)
(macrolet ((def (name offset)
             `(defmacro ,name (storage-vector)
                `(svref ,storage-vector ,,offset))))
  (def %metadata-table 0)
  (def new-vector 1)
  (def finished-copying 2)
  (def going-to-copy 3)
  (def table-count 4)
  (def table-slot-count 5)
  (def creation-time 6)
  (def %size 7))

(declaim (inline metadata-table storage-size))
(defun metadata-table (storage)
  (sb-ext:truly-the metadata-vector
                    (%metadata-table storage)))
(defun storage-size (storage)
  (sb-ext:truly-the fixnum (%size storage)))

(macrolet ((def (name offset)
             `(defmacro ,name (storage-vector n)
                `(svref ,storage-vector
                        (+ +words-before-values+ ,,offset (* ,n 2))))))
  (def key 0)
  (def value 1))

(defun make-storage-vector (size)
  (let ((storage (make-array (+ (* size 2) +words-before-values+)
                             :initial-element +empty+)))
    (setf (%metadata-table storage)  (make-metadata-vector size)
          (new-vector storage)       nil
          (finished-copying storage) 0
          (going-to-copy storage)    0
          (table-count storage)      (make-counter)
          (table-slot-count storage) (make-counter)
          (creation-time storage)    (get-internal-real-time)
          (%size storage)            size)
    storage))

(defun nearest-allowed-size (size)
  (max +metadata-entries-per-group+
       ;; This form returns the next power of 2 above SIZE.
       (expt 2 (integer-length (1- size)))))
