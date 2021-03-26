(in-package :sb-vm)

(defknown threadmill::%avx2-broadcast ((unsigned-byte 8))
    (sb-ext:simd-pack integer)
    (foldable movable flushable))

(threadmill::define-boring-vop threadmill::%avx2-broadcast
    ((byte unsigned-num :scs (unsigned-reg)))
    (broadcasted simd-pack-int :scs (int-sse-reg))
  (inst movq broadcasted byte)
  (inst vpbroadcastb broadcasted broadcasted))

(in-package :threadmill)

(sb-alien:define-alien-variable avx2-supported sb-alien:int)
