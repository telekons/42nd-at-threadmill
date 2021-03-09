;;; According to a presentation by an Amazon Web Services engineer,
;;; one cannot design an in-memory database using a garbage collected
;;; language implementation, because it would be too slow due to
;;; collection.  He then goes to use a hash table with just one lock,
;;; copies the data to be stored just 'cause, finds performance similar
;;; to Redis, and calls it a day.

;;; Using structure sharing and a concurrent hash table, we can go
;;; much faster.  Perhaps a magnitude or two faster - not that I'm
;;; really taking the problem seriously, by avoiding network
;;; serialisation, and even inter-thread mailboxes; but this should
;;; show that tasteful use of concurrent data structures makes things
;;; go fast.

(defpackage :phony-redis
  (:use :cl)
  (:export #:make-server #:connect-to-server
           #:find-value #:close-connection))
(in-package :phony-redis)

(defmacro specialize (string body)
  "Convince the compiler to generate a fast path for simple strings."
  `(if (typep ,string '(simple-array character 1))
       ,body
       ,body))

(defun djb (string)
  (declare (string string)
           (optimize speed))
  (let ((hash 5381))
    (declare ((and unsigned-byte fixnum) hash))
    (specialize
     string
     (dotimes (n (min 6 (length string)))
       (setf hash
             (logand most-positive-fixnum
                     (logxor (* hash 33)
                             (char-code (schar string n)))))))
    hash))

(defun make-server ()
  (threadmill:make-hash-table
   :test #'equal
   :hash-function #'djb
   :size 512))

(defun connect-to-server (server)
  server)

(defun find-value (connection name)
  (threadmill:gethash name connection))

(defun (setf find-value) (value connection name)
  (setf (threadmill:gethash name connection)
        value))

(defun close-connection (connection)
  (declare (ignore connection))
  (values))
