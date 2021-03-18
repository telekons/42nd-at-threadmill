(in-package :threadmill)

(sb-c:defknown %avx2-broadcast ((unsigned-byte 8))
    (sb-ext:simd-pack integer)
    (sb-c:foldable sb-c:movable sb-c:flushable))

(in-package :sb-vm)

(define-vop (threadmill::%avx2-broadcast)
  (:translate threadmill::%avx2-broadcast)
  (:policy :fast-safe)
  (:args (byte :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:temporary (:sc int-sse-reg) temp)
  (:results (broadcasted :scs (int-sse-reg)))
  (:result-types simd-pack-int)
  (:generator 0
              (inst movq temp byte)
              (inst vpbroadcastb broadcasted temp)))

(in-package :threadmill)

(defun %avx2-broadcast (byte)
  (%avx2-broadcast byte))

(sb-alien:define-alien-variable avx2-supported sb-alien:int)
