(in-package :threadmill)

(sb-c:defknown bsf ((unsigned-byte 64))
    (unsigned-byte 64)
    (sb-c:foldable sb-c:movable sb-c:flushable))

(in-package :sb-vm)

(define-vop (threadmill::bsf)
  (:translate threadmill::bsf)
  (:policy :fast-safe)
  (:args (value :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (scan :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 0
              (inst bsf scan value)))

(in-package :threadmill)

(defun bsf (x)
  (bsf x))

(declaim (inline bsf/16))
(defun bsf/16 (x)
  (declare ((unsigned-byte 16) x))
  (sb-ext:truly-the (mod 16) (bsf x)))

(declaim (inline bsf/32))
(defun bsf/32 (x)
  (declare ((unsigned-byte 32) x))
  (sb-ext:truly-the (mod 32) (bsf x)))
