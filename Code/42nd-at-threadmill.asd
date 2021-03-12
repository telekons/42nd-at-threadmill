(asdf:defsystem :42nd-at-threadmill
  :depends-on (:atomics :cl-simd :bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "bsf")
               (:file "counter")
               (:file "sse-metadata")
               (:file "storage-vector")
               (:file "hash-table-defstruct")
               (:file "hash-table")
               (:file "resize")))
