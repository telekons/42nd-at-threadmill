(in-package :sb-x86-64-asm)

;;; Patch around l. 870 of src/compiler/x86-64/avx2-insts.lisp
(macrolet ((def (name prefix opcode &key reg-only)
             `(define-instruction ,name (segment dst src &optional (src-size :oword))
                ,@(avx2-inst-printer-list 'reg-ymm/mem prefix opcode)
                (:emitter
                 (aver (gpr-p dst))
                 ,(when reg-only
                    `(aver (xmm-register-p src)))
                 (let ((dst-size (operand-size dst)))
                   (aver (or (eq dst-size :qword) (eq dst-size :dword)))
                   (emit-avx2-inst segment src dst ,prefix ,opcode
                                   :w (ecase dst-size
                                        (:qword 1)
                                        (:dword 0))
                                   :l (ecase src-size
                                        (:oword 0)
                                        (:hword 1))))))))
  (def vcvtsd2si  #xf2 #x2d)
  (def vcvtss2si  #xf3 #x2d)
  (def vcvttsd2si #xf2 #x2c)
  (def vcvttss2si #xf3 #x2c)
  (def vmovmskpd  #x66 #x50 :reg-only t)
  (def vmovmskps  nil  #x50 :reg-only t)
  (def vpmovmskb  #x66 #xd7 :reg-only t))

(in-package :sb-vm)

(defknown threadmill::%avx2-load
    ((simple-array (unsigned-byte 8) (*)) (unsigned-byte 64))
    (simd-pack-256 integer)
    (foldable flushable)
  :overwrite-fndb-silently t)

(threadmill::define-boring-vop threadmill::%avx2-load
    ((vector simple-array-unsigned-byte-8 :scs (descriptor-reg))
     (index  unsigned-num :scs (unsigned-reg)))
    (bytes simd-pack-256-int :scs (int-avx2-reg))
  (inst vmovdqu bytes
        (ea (- (* vector-data-offset n-word-bytes)
               other-pointer-lowtag)
            vector index 1)))

(defknown threadmill::%avx2-movemask
    ((simd-pack-256 integer))
    (unsigned-byte 32)
    (foldable movable flushable)
  :overwrite-fndb-silently t)

(threadmill::define-boring-vop threadmill::%avx2-movemask
    ((pack simd-pack-256-int :scs (int-avx2-reg)))
    (mask unsigned-num :scs (unsigned-reg))
  (inst vpmovmskb mask pack :hword))

(defknown threadmill::%avx2-broadcast/256 ((unsigned-byte 8))
    (simd-pack-256 integer)
    (foldable movable flushable)
  :overwrite-fndb-silently t)

(threadmill::define-boring-vop threadmill::%avx2-broadcast/256
    ((byte unsigned-num :scs (unsigned-reg)))
    (broadcasted simd-pack-256-int :scs (int-avx2-reg))
  (inst movq broadcasted byte)
  (inst vpbroadcastb broadcasted broadcasted))

(defknown threadmill::%avx2= ((simd-pack-256 integer) (simd-pack-256 integer))
    (simd-pack-256 integer)
    (foldable movable flushable)
  :overwrite-fndb-silently t)

(threadmill::define-boring-vop threadmill::%avx2=
    ((pack1 simd-pack-256-int :scs (int-avx2-reg))
     (pack2 simd-pack-256-int :scs (int-avx2-reg)))
    (equals simd-pack-256-int :scs (int-avx2-reg))
  (inst vpcmpeqb equals pack1 pack2))
