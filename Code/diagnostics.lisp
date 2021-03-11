(in-package :threadmill)

(defun group-sizes (table)
  (let* ((storage  (hash-table-storage table))
         (metadata (metadata-table storage))
         (histogram (make-array (1+ +metadata-entries-per-group+)
                                :element-type '(unsigned-byte 64)
                                :initial-element 0)))
    (loop for group-start below (length metadata)
          by +metadata-entries-per-group+
          for count = (count +empty-metadata+ metadata
                             :start group-start
                             :end (+ group-start +metadata-entries-per-group+)
                             :test #'/=)
          do (incf (aref histogram count)))
    histogram))

(defun h2-counts (table)
  (let* ((storage  (hash-table-storage table))
         (metadata (metadata-table storage))
         (histogram (make-array 128
                                :element-type '(unsigned-byte 64)
                                :initial-element 0)))
    (loop for value across metadata
          when (< value #x80)
            do (incf (aref histogram value)))
    histogram))
