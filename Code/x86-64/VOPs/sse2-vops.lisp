(in-package :sb-vm)

(defknown threadmill::%sse2-load
    ((simple-array (unsigned-byte 8) (*)) (unsigned-byte 64))
    (simd-pack integer)
    (foldable flushable)
  :overwrite-fndb-silently t)

(threadmill::define-boring-vop threadmill::%sse2-load
    ((vector simple-array-unsigned-byte-8 :scs (descriptor-reg))
     (index  unsigned-num :scs (unsigned-reg)))
    (bytes simd-pack-int :scs (int-sse-reg))
  ;; SBCL vectors are aligned to 16 bytes.
  (inst movdqa bytes
        (ea (- (* vector-data-offset n-word-bytes)
               other-pointer-lowtag)
            vector index 1)))
(defknown threadmill::%sse2-broadcast-byte
    ((unsigned-byte 8))
    (simd-pack integer)
    (foldable movable flushable)
  :overwrite-fndb-silently t)

(threadmill::define-boring-vop threadmill::%sse2-broadcast-byte
    ((byte unsigned-num :scs (unsigned-reg)))
    (bytes simd-pack-int :scs (int-sse-reg))
  (inst movd bytes byte)                ; xxxxxxxxxxxxxxxB
  (inst punpcklbw bytes bytes)          ; xxxxxxxxxxxxxxBB
  (inst punpcklbw bytes bytes)          ; xxxxxxxxxxxxBBBB
  (inst pshufd bytes bytes #4r0000))    ; BBBBBBBBBBBBBBBB

(defknown threadmill::%sse2-movemask
    ((simd-pack integer))
    (unsigned-byte 16)
    (foldable movable flushable)
  :overwrite-fndb-silently t)

(threadmill::define-boring-vop threadmill::%sse2-movemask
    ((bytes simd-pack-int :scs (int-sse-reg)))
    (mask unsigned-num :scs (unsigned-reg))
  (inst pmovmskb mask bytes))

(defknown threadmill::%sse2=
    ((simd-pack integer) (simd-pack integer))
    (simd-pack integer)
    (foldable movable flushable)
  :overwrite-fndb-silently t)

(threadmill::define-boring-vop threadmill::%sse2=
    ((a simd-pack-int :scs (int-sse-reg) :target result)
     (b simd-pack-int :scs (int-sse-reg)))
    (result simd-pack-int :scs (int-sse-reg))
  (unless (location= a result)
    (inst movdqa result a))
  (inst pcmpeqb result b))
