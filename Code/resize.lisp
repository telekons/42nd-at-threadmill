(in-package :threadmill)

(defun too-new-p (hash-table)
  "Was the last resize performed recently?"
  ;; If the last resize was done in the last 10ms, we probably should grow,
  ;; regardless of load factor.
  (< (creation-time (hash-table-storage hash-table))
     (- (get-internal-real-time)
        (* 0.01 internal-time-units-per-second))))

(defun help-copy (hash-table storage)
  ;; If the storage vector was already swapped out, bail out.
  (unless (eq storage (hash-table-storage hash-table))
    (return-from help-copy))
  (let* ((old-size (hash-table-size hash-table))
         (new-size
           (if (or (too-new-p hash-table)
                   (> (/ (hash-table-count hash-table)
                         (float old-size))
                      0.5))
               (* old-size 2)
               old-size))
         (hash-function (hash-table-hash hash-table))
         (own-vector nil))
    #+(or)
    (format t "~&Resizing from ~d to ~d"
            old-size new-size)
    (loop
      (let ((new-vector (new-vector storage)))
        (unless (null new-vector)
          (return-from help-copy
            (copy-into storage new-vector hash-function hash-table)))
        (when (null own-vector)
          (setf own-vector (make-storage-vector new-size)))
        (when (atomics:cas (new-vector storage) nil own-vector)
          (return-from help-copy
            (copy-into storage own-vector hash-function hash-table)))))))

;;; Copying is done in "segments". Each thread repeatedly claims
;;; segments of the storage vector to copy into the new vector, until there
;;; are no more segments to copy.
(defconstant +segment-size+ 1024)

(defun next-segment-to-copy (storage size)
  (loop for old-value = (going-to-copy storage)
        for new-value = (+ old-value +segment-size+)
        do (when (>= old-value size)
             ;; Nothing more to copy.
             (return (values 0 nil)))
           (when (atomics:cas (going-to-copy storage)
                              old-value new-value)
             (return (values old-value t)))))

(defun copy-into (old-storage new-storage hash-function hash-table)
  (let* ((metadata-table (metadata-table old-storage))
         (size           (length metadata-table)))
    (loop
      (multiple-value-bind (start present?)
          (next-segment-to-copy old-storage size)
        (unless present?
          (return))
        (copy-segment old-storage metadata-table new-storage
                      start size hash-function)
        (loop for old-value = (finished-copying old-storage)
              for new-value = (+ old-value +segment-size+)
              until (atomics:cas (finished-copying old-storage)
                                 old-value (+ old-value +segment-size+))
              ;; When we copied the last segment, install the new table.
              finally (when (>= new-value size)
                        (atomic-setf (hash-table-storage hash-table)
                                     new-storage)
                        (return-from copy-into)))))))

(defun copy-segment (old-storage metadata new-storage start size hash-function)
  (loop for position from start
          below (min size (+ start +segment-size+))
        do (let ((k (key old-storage position))
                 (v (value old-storage position)))
             ;; Grab the last value stored here.
             (loop until (atomics:cas (value old-storage position)
                                      v +copied+)
                   do (setf v (value old-storage position)))
             (unless (or (eq k +empty+)
                         (eq v +empty+))
               ;; Store it in the new table.
               (store-copied-value new-storage metadata hash-function k v)))))

(defun store-copied-value (storage metadata hash-function key value)
  ;; Copying should never store duplicate keys. We exploit this to
  ;; avoid testing keys, instead only copying into new entries.
  (dx-flet ((test (this-key)
              (declare (ignore this-key))
              t)
            (mask (group metadata)
              (declare (ignore metadata))
              (writable group))
            (consume (this-key position h2)
              (declare (ignore this-key))
              (when (claim-key storage key +empty+ position)
                (loop until (atomics:cas #1=(value storage position)
                                         #1# value))
                (increment-counter (table-count storage))
                (atomic-setf (metadata metadata position) h2))))
    (call-with-positions storage metadata
                         (funcall hash-function key)
                         #'test #'mask #'consume)))
