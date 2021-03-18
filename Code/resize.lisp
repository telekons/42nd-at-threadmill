(in-package :threadmill)

(defun too-new-p (hash-table)
  "Was the last resize performed recently?"
  ;; If the last resize was done in the last second, we probably should grow,
  ;; regardless of load factor.
  ;; This heuristic is...somehow not easy to reason with, and makes things
  ;; slower most of the time.
  (< (creation-time (hash-table-storage hash-table))
     (- (get-internal-real-time)
        (* 1.000 internal-time-units-per-second))))

(defun help-copy (hash-table storage)
  ;; If the storage vector was already swapped out, bail out.
  (unless (eq storage (hash-table-storage hash-table))
    (return-from help-copy))
  (let* ((old-size (hash-table-size hash-table))
         (new-size
           (if (or (> (/ (hash-table-count hash-table)
                         (float old-size))
                      (hash-table-rehash-threshold hash-table))
                   (> (/ (hash-table-count hash-table)
                         (float (counter-value (table-slot-count storage))))
                      0.75))
               (* old-size 2)
               old-size))
         (megabytes (ash (* new-size 16) -20)))
    (flet ((continuation (new-vector)
             (return-from help-copy
               (copy-into storage new-vector hash-table))))
      (unless (null (new-vector storage))
        (continuation (new-vector storage)))
      ;; Offer to make the new vector.
      (when (atomics:cas (allocating-new-p storage) nil t)
        #+log-copying
        (format t "~&Creating a ~d element storage vector" new-size)
        (let ((new-vector (make-storage-vector new-size)))
          (atomic-setf (new-vector storage) new-vector)
          (continuation new-vector)))
      ;; Else, wait for another thread to create the new vector.
      (loop while (null (new-vector storage))
            do (sleep (* 8e-5 megabytes)))
      (continuation (new-vector storage)))))

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

(defun report-finished-copying (new-storage)
  (declare (ignorable new-storage))
  #+log-copying
  (format t "~&Finished copying ~d entries after ~8e seconds"
          (counter-value (table-count new-storage))
          (/ (- (get-internal-real-time) (creation-time new-storage))
             internal-time-units-per-second)))

(defun copy-into (old-storage new-storage hash-table)
  (let ((metadata-table (metadata-table new-storage))
        (hash-function  (hash-table-hash hash-table))
        (size           (length (metadata-table old-storage))))
    (loop
      (multiple-value-bind (start present?)
          (next-segment-to-copy old-storage size)
        (unless present?
          (bt:thread-yield)
          (return))
        (copy-segment hash-table
                      old-storage metadata-table new-storage
                      start size hash-function)
        ;; Bump the copy progress.
        (loop for old-value = (finished-copying old-storage)
              for new-value = (+ old-value +segment-size+)
              until (atomics:cas (finished-copying old-storage)
                                 old-value (+ old-value +segment-size+))
              ;; When we copied the last segment, install the new table.
              ;; Note that if a thread creates a recursive copy (which is
              ;; definitely going to be linked here before we return from
              ;; COPY-SEGMENT), we do not install the new table.
              finally (when (>= new-value size)
                        (when (null (new-vector new-storage))
                          (atomic-setf (hash-table-storage hash-table)
                                       new-storage))
                        (report-finished-copying new-storage)
                        (return-from copy-into)))))))

(defun copy-segment (hash-table old-storage metadata new-storage
                     start size hash-function)
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
               (store-copied-value hash-table
                                   new-storage metadata
                                   (funcall hash-function k)
                                   k v size)))))

(defun store-copied-value (hash-table storage metadata hash key value size)
  "Attempt to copy a key and value."
  (declare (hash-table hash-table)
           (simple-vector storage)
           (metadata-vector metadata)
           (fixnum size)
           (fixnum hash)
           #.+optimizations+)
  ;; Copying should never store duplicate keys. We exploit this to
  ;; avoid testing keys, instead only copying into new entries.
  (dx-labels ((test (this-key)
                (declare (ignore this-key))
                t)
              (mask (group metadata)
                (declare (ignore metadata))
                (writable group))
              (consume (this-key position h2)
                (declare (ignore this-key))
                (loop for old-key = (key storage position)
                      do (unless (eq old-key +empty+)
                           (return-from consume))
                         (when (atomics:cas (key storage position)
                                            +empty+ key)
                           (return)))
                (atomic-setf (metadata metadata position) h2)
                (loop for old-value = (value storage position)
                      do (when (eq old-value +copied+)
                           (return-from store-copied-value
                             (recursive-copy hash-table storage
                                             hash key value size)))
                         (when (atomics:cas (value storage position)
                                            old-value value)
                           (return)))
                (increment-counter (table-count storage))
                (return-from store-copied-value)))
    (call-with-positions storage metadata hash
                         #'test #'mask #'consume)
    (recursive-copy hash-table storage hash key value size)))

(defun recursive-copy (hash-table storage hash key value size)
  (flet ((continuation (new-storage)
           (let ((new-metadata (metadata-table new-storage)))
             (store-copied-value hash-table new-storage new-metadata
                                 hash key value (length new-metadata))
             (copy-into storage new-storage hash-table))))
    (loop
      (unless (null (new-vector storage))
        (return (continuation (new-vector storage))))
      (let ((new-storage (make-storage-vector (* 2 size))))
        (when (atomics:cas (new-vector storage) nil new-storage)
          #+log-copying
          (warn "Failed to copy; doing a recursive copy.")
          (return (continuation new-storage)))))))
