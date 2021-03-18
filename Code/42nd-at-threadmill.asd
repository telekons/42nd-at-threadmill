(asdf:defsystem :42nd-at-threadmill
  :depends-on (:atomics :cl-simd :bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:module "x86-64"
                :components
                ((:module "VOPs"
                  :components
                  ((:file "avx2-broadcastb")
                   (:file "bsf")
                   (:file "cas-bytes")))
                 (:file "sse-metadata")))
               (:file "counter")
               (:file "storage-vector")
               (:file "hash-table-defstruct")
               (:file "hash-table")
               (:file "resize")))
