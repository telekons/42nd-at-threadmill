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
    (foldable movable flushable)
  :overwrite-fndb-silently t)

(define-vop (threadmill::%avx2-load)
  (:translate threadmill::%avx2-load)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg))
         (index  :scs (unsigned-reg)))
  (:arg-types simple-array-unsigned-byte-8 unsigned-num)
  (:results (bytes :scs (int-avx2-reg)))
  (:result-types simd-pack-256-int)
  (:generator 0
     (inst vmovdqu bytes
           (ea (- (* vector-data-offset n-word-bytes)
                  other-pointer-lowtag)
               vector index 1))))

(defknown threadmill::%avx2-movemask
    ((simd-pack-256 integer))
    (unsigned-byte 32)
    (foldable movable flushable)
  :overwrite-fndb-silently t)

(define-vop (threadmill::%avx2-movemask)
  (:translate threadmill::%avx2-movemask)
  (:policy :fast-safe)
  (:args (pack :scs (int-avx2-reg)))
  (:arg-types simd-pack-256-int)
  (:results (mask :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 0
     (inst vpmovmskb mask pack :hword)))

(defknown threadmill::%avx2-broadcast/256 ((unsigned-byte 8))
    (simd-pack-256 integer)
    (foldable movable flushable)
  :overwrite-fndb-silently t)

(define-vop (threadmill::%avx2-broadcast/256)
  (:translate threadmill::%avx2-broadcast/256)
  (:policy :fast-safe)
  (:args (byte :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:temporary (:sc int-avx2-reg) temp)
  (:results (broadcasted :scs (int-avx2-reg)))
  (:result-types simd-pack-256-int)
  (:generator 0
     (inst movq temp byte)
     (inst vpbroadcastb broadcasted temp)))

(defknown threadmill::%avx2= ((simd-pack-256 integer) (simd-pack-256 integer))
    (simd-pack-256 integer)
    (foldable movable flushable)
  :overwrite-fndb-silently t)

(define-vop (threadmill::%avx2=)
  (:translate threadmill::%avx2=)
  (:policy :fast-safe)
  (:args (pack1 :scs (int-avx2-reg))
         (pack2 :scs (int-avx2-reg)))
  (:arg-types simd-pack-256-int simd-pack-256-int)
  (:results (equals :scs (int-avx2-reg)))
  (:result-types simd-pack-256-int)
  (:generator 0
     (inst vpcmpeqb equals pack1 pack2)))

(in-package :threadmill)

(defun %avx2-load (ub8-vector index)
  (%avx2-load ub8-vector index))

(defun %avx2-movemask (pack)
  (%avx2-movemask pack))

(defun %avx2-broadcast/256 (byte)
  (%avx2-broadcast/256 byte))

(defun %avx2= (pack1 pack2)
  (%avx2= pack1 pack2))
