(asdf:defsystem :42nd-at-threadmill
  :depends-on (:atomics :bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "counter")
               (:file "storage-vector")
               (:file "hash-table-defstruct")
               (:file "hash-table")
               (:file "resize")))
