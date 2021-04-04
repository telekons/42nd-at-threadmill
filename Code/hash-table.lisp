(in-package :threadmill)

(defconstant +empty+ '+empty+)
(defconstant +copied+ '+copied+)
;; SBCL seems to have difficulties proving that we won't go out of
;; bounds. Fair enough, we have a funny way of addressing the table,
;; but we don't want to do bounds checks.
(alexandria:define-constant +optimizations+
    '(optimize (speed 3) (sb-c::insert-array-bounds-checks 0))
  :test #'equal)
(deftype vector-index ()
  '(and fixnum unsigned-byte))

(declaim (inline cheap-mod))
(defun cheap-mod (number divisor)
  "A cheap and usually incorrect MOD, which works when DIVISOR is a power of two."
  (logand number (1- divisor)))

(defconstant +probe-limit+ 32
  "The maximum number of entries to probe.")

(declaim (inline call-with-positions))
(defun call-with-positions (storage hash
                            test continuation)
  (declare (function test continuation)
           (fixnum hash)
           (simple-vector storage)
           (optimize (speed 3) (safety 0)))
  (let* ((probed 0)
         (length         (storage-vector-size storage))
         (probe-limit    (min length +probe-limit+))
         (probe-position (cheap-mod hash length)))
    (declare (vector-index probe-position)
             (fixnum probed))
    (loop
      (let* ((this-key (key storage probe-position)))
        (when (funcall test this-key)
          (funcall continuation this-key probe-position))
        (when (eq this-key +empty+)
          (return-from call-with-positions)))
      (incf probed)
      (setf probe-position (cheap-mod (1+ probe-position) length))
      (when (>= probed probe-limit)
        (return-from call-with-positions)))))

(defmacro dx-labels (definitions &body body)
  (let ((names (mapcar #'first definitions)))
    `(labels ,definitions
       (declare (inline ,@names)
                (sb-int:truly-dynamic-extent
                 ,@(loop for name in names collect `#',name)))
       ,@body)))

(defun gethash (key hash-table &optional (default nil))
  (declare (hash-table hash-table)
           #.+optimizations+)
  (let* ((storage (hash-table-storage hash-table))
         (hash (funcall (hash-table-hash hash-table) key))
         (test-function (hash-table-test hash-table)))
    (dx-labels ((test (this-key)
                  (or (eq this-key key)
                      (funcall test-function this-key key)))
                (consume (this-key position)
                  (declare (ignore this-key))
                  (let ((value (value storage position)))
                    (when (eq value +empty+)
                      (return-from gethash (values default nil)))
                    (when (eq value +copied+)
                      (help-copy hash-table storage)
                      (return-from gethash (gethash key hash-table)))
                    (return-from gethash
                      (values value t)))))
      (call-with-positions storage hash #'test #'consume)
      (values default nil))))

(declaim (inline claim-key))
(defun claim-key (storage key this-key position test)
  "Attempt to claim a position in the table, returning values:
NIL, NIL if another thread claimed it for another key first
T,   NIL if this position already was claimed with this key
T,   T   if we successfully claimed this position"
  (declare (optimize (speed 3) (safety 0))
           (vector-index position)
           (simple-vector storage)
           (function test))
  (loop
    (unless (eq this-key +empty+)
      (when (or (eq this-key key)
                (funcall test this-key key))
        (return-from claim-key (values t nil)))
      (return-from claim-key (values nil nil)))
    (when (atomics:cas (key storage position) +empty+ key)
      (increment-counter (table-slot-count storage))
      (return-from claim-key (values t t)))
    (setf this-key (key storage position))))

(defun (setf gethash) (new-value key hash-table &optional default)
  (declare (ignore default)
           #.+optimizations+)
  (let* ((storage (hash-table-storage hash-table))
         (hash (funcall (hash-table-hash hash-table) key))
         (test-function (hash-table-test hash-table)))
    (dx-labels ((lose-and-resize ()
                  (help-copy hash-table storage)
                  (return-from gethash
                    (setf (gethash key hash-table) new-value)))
                (test (this-key)
                  (or (eq this-key key)
                      (eq this-key +empty+)
                      (funcall test-function this-key key)))
                (consume (this-key position)
                  (multiple-value-bind (ours? new?)
                      (claim-key storage key this-key position test-function)
                    (unless ours?
                      ;; Another thread got this position.
                      (return-from consume))
                    (loop for old-value = (value storage position)
                          do (when (eq old-value +copied+)
                               (lose-and-resize))
                             (when (eq old-value new-value)
                               ;; Sure, that'll do.
                               (return))
                             (when (atomics:cas (value storage position)
                                                old-value new-value)
                               (when (eq old-value +empty+)
                                 (increment-counter (table-count storage)))
                               (return)))
                    (return-from gethash new-value))))
      (call-with-positions storage hash #'test #'consume)
      (lose-and-resize))))

(defun modhash (key hash-table modifier)
  (declare (function modifier)
           #.+optimizations+)
  (let* ((storage (hash-table-storage hash-table))
         (hash (funcall (hash-table-hash hash-table) key))
         (test-function (hash-table-test hash-table)))
    (dx-labels ((test (this-key)
                  (or (eq this-key key)
                      (eq this-key +empty+)
                      (funcall test-function this-key key)))
                (consume (this-key position)
                ;;; This wastes a slot if we transition from empty -> empty.
                ;;; Otherwise it's less of a hassle to implement just this one
                ;;; function rather than PUT-IF-MATCH, PUT-IF-ABSENT, etc.
                  (multiple-value-bind (ours? new?)
                      (claim-key storage key this-key position test-function)
                    (unless ours?
                      ;; Another thread got this position.
                      (return-from consume))
                    (loop
                      (let ((value (value storage position)))
                        (when (eq value +copied+)
                          (help-copy hash-table storage)
                          (return-from modhash
                            (modhash key hash-table modifier)))
                        (multiple-value-bind (new-value new-present?)
                            (funcall modifier
                                     value (not (eq value +empty+)))
                          (cond
                            (new-present?
                             (when (eq value new-value)
                               ;; Nothing to do.
                               (return-from modhash))
                             (when (atomics:cas (value storage position)
                                                value new-value)
                               ;; We only increment if we just brought this slot
                               ;; to life.
                               (when (eq value +empty+)
                                 (increment-counter (table-count storage)))
                               (return-from modhash)))
                            (t
                             (when (eq value +empty+)
                               ;; Nothing to do.
                               (return-from modhash))
                             (when (atomics:cas (value storage position)
                                                value +empty+)
                               (unless (eq value +empty+)
                                 (decrement-counter (table-count storage)))
                               (return-from modhash))))))))))
      (call-with-positions storage hash #'test #'consume))
    (help-copy hash-table storage)
    (return-from modhash
      (modhash key hash-table modifier))))

(defun remhash (key hash-table)
  (declare #.+optimizations+)
  (let* ((storage (hash-table-storage hash-table))
         (hash (funcall (hash-table-hash hash-table) key))
         (test-function (hash-table-test hash-table)))
    (dx-labels ((test (this-key)
                  (or (eq this-key key)
                      (funcall test-function this-key key)))
                (consume (this-key position)
                  (declare (ignore this-key))
                  (loop for last-value = (value storage position)
                        do (when (eq last-value +empty+)
                             ;; We didn't succeed if someone else removed the
                             ;; entry first.
                             (return-from remhash nil))
                           (when (eq last-value +copied+)
                             (help-copy hash-table storage)
                             (return-from remhash
                               (remhash key hash-table)))
                           (when (atomics:cas (value storage position)
                                              last-value +empty+)
                             (return)))
                  (decrement-counter (table-count storage))
                  (return-from remhash t)))
      (call-with-positions storage hash #'test #'consume)
      nil)))

(defun maphash (function hash-table)
  (declare (function function))
  (loop for storage = (hash-table-storage hash-table)
        until (null (new-vector storage))
        do (help-copy hash-table storage))
  (tagbody try-again
     ;; Try to copy out the hash table contents to an alist.
     (let* ((storage (hash-table-storage hash-table))
            (length (hash-table-size hash-table))
            (alist '()))
       (dotimes (n length)
         (let ((k (key   storage n))
               (v (value storage n)))
           (unless (or (eq k +empty+)
                       (eq v +empty+))
             (when (eq v +copied+)
               (help-copy hash-table storage)
               (go try-again))
             (push (cons k v) alist))))
       (mapc (lambda (pair)
               (funcall function
                        (car pair)
                        (cdr pair)))
             alist)))
  hash-table)

(defun hash-table-count (hash-table)
  (counter-value (table-count (hash-table-storage hash-table))))
(defun hash-table-size (hash-table)
  (storage-vector-size (hash-table-storage hash-table)))
