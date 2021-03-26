(in-package :threadmill)

(defmacro define-boring-vop (name args result &body generator)
  `(progn
     (sb-vm::define-vop (,name)
       (:translate ,name)
       (:policy :fast-safe)
       (:args ,@(loop for (name nil . rest) in args
                      collect (cons name rest)))
       (:arg-types ,@(mapcar #'second args))
       (:results (,(first result) ,@(rest (rest result))))
       (:result-types ,(second result))
       (:generator 0 ,@generator))
     (defun ,name ,(mapcar #'first args)
       (,name ,@(mapcar #'first args)))))
