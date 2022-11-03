(asdf:defsystem :42nd-at-threadmill
  :depends-on (:atomics :bordeaux-threads :sb-simd)
  :serial t
  :components ((:file "package")
               (:module "x86-64"
                :components
                ((:module "VOPs"
                  :components
                  ((:file "define-boring-vop")
                   (:file "sse2-vops" :if-feature (:not :threadmill-avx2))
                   (:file "bsf")
                   (:file "cas-bytes")))
                 (:file "sse-metadata"
                  :if-feature (:not :threadmill-avx2))
                 (:file "avx2-metadata"
                  :if-feature :threadmill-avx2)))
               (:file "counter")
               (:file "storage-vector")
               (:file "hash-table-defstruct")
               (:file "hash-table")
               (:file "resize")))
