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
  (:result-types unsigned-byte-64)
  (:generator 1
              (inst bsf scan value)))

(in-package :threadmill)

(defun bsf (x)
  (bsf x))
