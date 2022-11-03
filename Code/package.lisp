(defpackage :threadmill
  (:use :cl)
  (:shadow #:make-hash-table
           #:hash-table #:hash-table-p #:hash-table-test
           #:hash-table-rehash-threshold
           #:hash-table-count #:hash-table-size
           #:gethash #:remhash #:clrhash #:maphash)
  (:export #:make-hash-table #:hash-table #:hash-table-p
           #:hash-table-test #:hash-table-count #:hash-table-size
           #:gethash #:remhash #:clrhash #:maphash #:modhash)
  (:local-nicknames (#:avx2 #:sb-simd-avx2)
                    (#:sse2 #:sb-simd-sse2)))
