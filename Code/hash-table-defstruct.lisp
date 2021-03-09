(in-package :threadmill)

(defstruct (hash-table (:constructor %make-hash-table))
  (hash (error "no hash function")
   :type function
   :read-only t)
  (test (error "no test function")
   :type function
   :read-only t)
  (storage (error "no storage")
   :type simple-vector))

(defun make-hash-table (&key (test #'eql) (size 64) (hash-function #'sxhash)
                        &allow-other-keys)
  (assert (plusp size))
  (let* ((hash-function (alexandria:ensure-function hash-function))
         (test-function (alexandria:ensure-function test))
         (storage (make-storage-vector (nearest-allowed-size size))))
    (%make-hash-table :hash hash-function
                      :test test-function
                      :storage storage)))

(defmethod print-object ((table hash-table) stream)
  (print-unreadable-object (table stream :type t :identity t)
    (format stream "~s ~s load ~d/~d"
            :test
            (hash-table-test table)
            (hash-table-count table)
            (hash-table-size table))))
