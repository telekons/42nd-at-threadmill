(in-package :threadmill)

;;; Cliff Click uses a counter which self-resizes, not unlike the hash table
;;; itself, but that's kinda messy.
(defconstant +counter-slots+ 32)
(deftype counter ()
  `(simple-array sb-ext:word (,+counter-slots+)))
(deftype counter-offset ()
  `(mod ,+counter-slots+))

(defun make-counter ()
  (make-array +counter-slots+
              :initial-element 0
              :element-type 'sb-ext:word))

(defvar *counter-offset* (random +counter-slots+))
(declaim (sb-ext:always-bound *counter-offset*)
         (counter-offset *counter-offset*))

(push (cons '*counter-offset* '(random +counter-slots+))
      bt:*default-special-bindings*)

(declaim (inline change-counter increment-counter decrement-counter))
(defun change-counter (counter Δ)
  (declare (counter counter))
  (atomics:atomic-incf (aref counter *counter-offset*) Δ))

(defun increment-counter (counter)
  (change-counter counter 1))
(defun decrement-counter (counter)
  (change-counter counter -1))
(defun counter-value (counter)
  (declare (counter counter))
  (let ((sum 0))
    (loop for value across counter
          do (setf sum (ldb (byte 64 0) (+ sum value))))
    ;; If the sum has the MSB set, we either have a very, very large
    ;; table (Ed.: we don't) or the count is negative due to timing
    ;; weirdness. Just say the value is 0 in that case. 
    (if (logtest (ash 1 63) sum)
        0
        sum)))
