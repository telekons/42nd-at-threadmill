(in-package :threadmill)

;;; Cliff Click uses a counter which self-resizes, not unlike the hash table
;;; itself, but that's kinda messy.
(defconstant +counter-slots+ 32)
(deftype counter ()
  `(simple-vector ,+counter-slots+))
(deftype counter-offset ()
  `(mod ,+counter-slots+))

(defun make-counter ()
  (make-array +counter-slots+ :initial-element 0))

(defvar *counter-offset* (random +counter-slots+))
(declaim (sb-ext:always-bound *counter-offset*)
         (counter-offset *counter-offset*))

(push (cons '*counter-offset* '(random +counter-slots+))
      bt:*default-special-bindings*)

(declaim (inline change-counter))
(defun change-counter (counter Δ)
  (declare (counter counter))
  (loop with offset = *counter-offset*
        for old-value = #1=(svref counter offset)
        until (atomics:cas #1# old-value (+ old-value Δ))))

(defun increment-counter (counter)
  (change-counter counter 1))
(defun decrement-counter (counter)
  (change-counter counter -1))
(defun counter-value (counter)
  (declare (counter counter))
  (reduce #'+ counter))
