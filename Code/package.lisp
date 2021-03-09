(defpackage :threadmill
  (:use :cl)
  (:shadow #:make-hash-table
           #:hash-table #:hash-table-p #:hash-table-test
           #:hash-table-count #:hash-table-size
           #:gethash #:remhash #:clrhash #:maphash)
  (:export #:make-hash-table #:hash-table #:hash-table-p
           #:hash-table-test #:hash-table-count #:hash-table-size
           #:gethash #:remhash #:clrhash #:maphash #:modhash))
