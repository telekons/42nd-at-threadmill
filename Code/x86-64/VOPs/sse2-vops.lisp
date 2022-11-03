(in-package :sb-vm)

; require this currently because sb-simd broadcast-byte really sucks
(defknown threadmill::%sse2-broadcast-byte
    ((unsigned-byte 8))
    (simd-pack (unsigned-byte 8))
    (foldable movable flushable)
  :overwrite-fndb-silently t)

(sb-c:define-vop (threadmill::%sse2-broadcast-byte)
  (:translate threadmill::%sse2-broadcast-byte)
  (:policy :fast-safe)
  (:args (byte :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (bytes :scs (int-sse-reg)))
  (:result-types simd-pack-ub8)
  (:temporary (:scs (int-sse-reg)) zero)
  (:generator 0
              (inst movd bytes byte)
              (inst pxor zero zero)
              (inst pshufb bytes zero)))
