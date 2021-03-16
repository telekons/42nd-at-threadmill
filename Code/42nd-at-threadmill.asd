(asdf:defsystem :42nd-at-threadmill
  :depends-on (:atomics :cl-simd :bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:module "VOPs"
                :components
                ((:file "avx2-broadcastb")
                 (:file "bsf")
                 (:file "cas-bytes")))
               (:file "counter")
               (:file "sse-metadata")
               (:file "storage-vector")
               (:file "hash-table-defstruct")
               (:file "hash-table")
               (:file "resize")))
