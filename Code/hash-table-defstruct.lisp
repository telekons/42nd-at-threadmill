(in-package :threadmill)

(defstruct (hash-table (:constructor %make-hash-table))
  (hash (error "no hash function")
   :type (function * fixnum)
   :read-only t)
  (test (error "no test function")
   :type function
   :read-only t)
  (storage (error "no storage")
   :type simple-vector)
  (load-factor 0.5s0
   :type single-float
   :read-only t))

(defun make-hash-table (&key (test #'eql) (size 64) (hash-function #'sxhash)
                             (load-factor 0.5)
                        &allow-other-keys)
  (assert (plusp size))
  (assert (plusp load-factor))
  ;; Plan to store SIZE entries with the given load factor.
  (let* ((size (round size load-factor))
         (hash-function (alexandria:ensure-function hash-function))
         (test-function (alexandria:ensure-function test))
         (storage (make-storage-vector (nearest-allowed-size size))))
    (%make-hash-table :hash hash-function
                      :test test-function
                      :storage storage
                      :load-factor (float load-factor 0.0s0))))

(defmethod print-object ((table hash-table) stream)
  (print-unreadable-object (table stream :type t :identity t)
    (format stream "~s ~s load ~d/~d"
            :test
            (hash-table-test table)
            (hash-table-count table)
            (hash-table-size table))))
