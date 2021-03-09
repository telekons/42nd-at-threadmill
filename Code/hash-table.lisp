(in-package :threadmill)

(defconstant +empty+ '+empty+)
(defconstant +copied+ '+copied+)

(declaim (inline split-hash cheap-mod))
(defun split-hash (hash)
  "Split a hash into two parts (called H1 and H2).
H1 is used to find a starting probe position in the table, and H2 is used as metadata for fast probing."
  (declare ((unsigned-byte 64) hash))
  (floor hash 256))
(defun cheap-mod (number divisor)
  "A cheap and usually incorrect MOD, which works when DIVISOR is a power of two."
  (logand number (1- divisor)))

(defconstant +probe-limit+ 8
  "The maximum number of groups to probe.")

(declaim (inline call-with-positions))
(defun call-with-positions (storage metadata hash
                            test mask-generator continuation)
  (declare (function test mask-generator continuation)
           (fixnum hash)
           (simple-vector storage)
           (metadata-vector metadata)
           (optimize (speed 3) (safety 0)))
  (multiple-value-bind (h1 h2)
      (split-hash hash)
    (let* ((probed 0)
           (groups         (metadata-groups metadata))
           (probe-limit    (min groups +probe-limit+))
           (probe-position (cheap-mod h1 groups)))
      (declare (metadata-index probe-position)
               (fixnum probed))
      (loop
        (let ((group (metadata-group metadata probe-position))
              (base-entry-position (* probe-position +metadata-entries-per-group+))
              (metadata (mask-h2 h2)))
          (do-matches (entry-offset (funcall mask-generator group metadata))
            (let* ((entry-position (+ entry-offset base-entry-position))
                   (this-key (key storage entry-position)))
              (when (funcall test this-key)
                (funcall continuation this-key entry-position metadata)))))
        (incf probed)
        (setf probe-position (cheap-mod (1+ probe-position) groups))
        (when (= probed probe-limit)
          (return-from call-with-positions))))))

(defmacro dx-flet (definitions &body body)
  (let ((names (mapcar #'first definitions)))
    `(flet ,definitions
       (declare (inline ,@names)
                (sb-int:truly-dynamic-extent
                 ,@(loop for name in names collect `#',name)))
       ,@body)))

(defun gethash (key hash-table &optional (default nil))
  (declare (hash-table hash-table)
           (optimize (speed 3)))
  (let* ((storage (hash-table-storage hash-table))
         (metadata (metadata-table storage))
         (hash (funcall (hash-table-hash hash-table) key))
         (test-function (hash-table-test hash-table)))
    (dx-flet ((test (this-key)
                (when (eq this-key +empty+)
                  (return-from gethash (values default nil)))
                (or (eq this-key key)
                    (funcall test-function this-key key)))
              (mask (group metadata)
                (match-union (writable group)
                             (bytes metadata group)))
              (consume (this-key position metadata)
                (declare (ignore this-key metadata))
                (let ((value (value storage position)))
                  (when (eq value +copied+)
                    (help-copy hash-table storage)
                    (return-from gethash (gethash key hash-table)))
                  (when (eq value +empty+)
                    (return-from gethash (values default nil)))
                  (return-from gethash
                    (values value t)))))
      (call-with-positions storage metadata
                           hash #'test #'mask #'consume)
      (values default nil))))

(declaim (inline claim-key))
(defun claim-key (storage key this-key position)
  "Attempt to claim a position in the table, returning values:
NIL, NIL if another thread claimed it for another key first
T,   NIL if this position already was claimed with this key
T,   T   if we successfully claimed this position"
  (declare (optimize (speed 3))
           (vector-index position))
  (loop
    (unless (or (eq this-key key)
                (eq this-key +empty+))
      (return-from claim-key (values nil nil)))
    (when (atomics:cas (key storage position) this-key key)
      (return-from claim-key (values t (eq this-key +empty+))))
    (setf this-key (key storage position))))

(defun (setf gethash) (new-value key hash-table &optional default)
  (declare (ignore default)
           (optimize (speed 3)))
  (let* ((storage (hash-table-storage hash-table))
         (metadata (metadata-table storage))
         (hash (funcall (hash-table-hash hash-table) key))
         (test-function (hash-table-test hash-table)))
    (dx-flet ((test (this-key)
                (or (eq this-key +empty+)
                    (eq this-key key)
                    (funcall test-function this-key key)))
              (mask (group metadata)
                (match-union (writable group)
                             (bytes metadata group)))
              (consume (this-key position h2)
                (multiple-value-bind (ours? new?)
                    (claim-key storage key this-key position)
                  (unless ours?
                    ;; Another thread got this position.
                    (return-from consume))
                  (loop for old-value = (value storage position)
                        do (when (eq old-value +copied+)
                             (help-copy hash-table storage)
                             (return-from gethash
                               (setf (gethash key hash-table) new-value)))
                           (when (atomics:cas (value storage position)
                                              old-value new-value)
                             (return)))
                  (when new?
                    (increment-counter (table-count storage))
                    (atomic-setf (metadata metadata position) h2))
                  (return-from gethash new-value))))
      (call-with-positions storage metadata
                           hash #'test #'mask #'consume))
    (help-copy hash-table storage)
    (return-from gethash
      (setf (gethash key hash-table) new-value))))

(defun modhash (key hash-table modifier)
  (declare (function modifier)
           (optimize (speed 3)))
  (let* ((storage (hash-table-storage hash-table))
         (metadata (metadata-table storage))
         (hash (funcall (hash-table-hash hash-table) key))
         (test-function (hash-table-test hash-table)))
    (dx-flet ((test (this-key)
                (or (eq this-key +empty+)
                    (eq this-key key)
                    (funcall test-function this-key key)))
              (mask (group metadata)
                (match-union (writable group)
                             (bytes metadata group)))
              (consume (this-key position h2)
                ;;; This wastes a slot if we transition from empty -> empty.
                ;;; Otherwise it's less of a hassle to implement just this one
                ;;; function rather than PUT-IF-MATCH, PUT-IF-ABSENT, etc.
                (multiple-value-bind (ours? new?)
                    (claim-key storage key this-key position)
                  (unless ours?
                    ;; Another thread got this position.
                    (return-from consume))
                  (when new?
                    (atomic-setf (metadata metadata position) h2))
                  (loop
                    (let ((value (value storage position)))
                      (when (eq value +copied+)
                        (return-from modhash
                          (modhash key hash-table modifier)))
                      (multiple-value-bind (new-value new-present?)
                          (funcall modifier
                                   value (not (eq value +empty+)))
                        (cond
                          (new-present?
                           ;; We only increment if we just brought this slot
                           ;; to life.
                           (when (atomics:cas (value storage position)
                                              value new-value)
                             (when new?
                               (increment-counter (table-count storage)))
                             (return-from modhash)))
                          (t
                           (when (atomics:cas (value storage position)
                                              value +empty+)
                             ;; We only decrement if we just killed this slot.
                             (unless new?
                               (decrement-counter (table-count storage)))
                             (return-from modhash))))))))))
      (call-with-positions storage metadata
                           hash #'test #'mask #'consume))
    (help-copy hash-table storage)
    (return-from modhash
      (modhash key hash-table modifier))))

(defun remhash (key hash-table)
  (let* ((storage (hash-table-storage hash-table))
         (metadata (metadata-table storage))
         (hash (funcall (hash-table-hash hash-table) key))
         (test-function (hash-table-test hash-table)))
    (dx-flet ((test (this-key)
                (when (eq this-key +empty+)
                  (return-from remhash nil))
                (or (eq this-key key)
                    (funcall test-function this-key key)))
              (mask (group metadata)
                (match-union (writable group)
                             (bytes metadata group)))
              (consume (this-key position h2)
                (declare (ignore this-key h2))
                (loop for last-value = (value storage position)
                      do (when (eq last-value +empty+)
                           ;; It's not really clear if we "succeeded" in
                           ;; removing an entry that someone else removed,
                           ;; but I guess only one thread can succeed.
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
      (call-with-positions storage metadata
                           hash #'test #'mask #'consume))
    nil))

(defun maphash (function hash-table)
  (declare (function function))
  (let* ((storage (hash-table-storage hash-table))
         (length (length (metadata-table storage))))
    ;; This procedure is lifted from Cliff Click's table, but it will certainly
    ;; not include copied entries.
    (dotimes (n length)
      (let ((k (key   storage n))
            (v (value storage n)))
        (unless (or (eq k +empty+)
                    (eq v +empty+)
                    (eq v +copied+))
          (funcall function k v))))))

(defun hash-table-count (hash-table)
  (counter-value (table-count (hash-table-storage hash-table))))
(defun hash-table-size (hash-table)
  (length (metadata-table (hash-table-storage hash-table))))
