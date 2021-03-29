(in-package :sb-vm)

(sb-c:defknown threadmill::%cas-byte
    ((simple-array (unsigned-byte 8) 1) (unsigned-byte 62) (unsigned-byte 8) (unsigned-byte 8))
    (unsigned-byte 8)
    ()
  :overwrite-fndb-silently t)

(define-vop (threadmill::%cas-byte)
  (:translate threadmill::%cas-byte)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :eval)
         (index :scs (unsigned-reg) :to :eval)
         (old-value :scs (unsigned-reg))
         (new-value :scs (unsigned-reg)))
  (:arg-types simple-array-unsigned-byte-8 unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset rax-offset
               :from (:argument 1) :to :result :target value)
              rax)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 5
              (move rax old-value)
              (inst cmpxchg :lock :byte
                    (ea (- (* vector-data-offset n-word-bytes)
                           other-pointer-lowtag)
                        object index 1)
                    new-value)
              (move value rax)))

(in-package :threadmill)

(defun %cas-byte (byte-vector index old new)
  (%cas-byte byte-vector index old new))

(declaim (inline cas-byte vector-cas-pair))
(defun cas-byte (byte-vector index old new)
  (declare ((simple-array (unsigned-byte 8) 1) byte-vector)
           ((unsigned-byte 8) old new)
           (optimize (speed 3) (safety 1)))
  (%cas-byte byte-vector
             (sb-kernel:check-bound byte-vector (length byte-vector) index)
             old new))
