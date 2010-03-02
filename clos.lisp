(in-package :com.gigamonkeys.utilities)

(defmacro with-slot-values ((&rest names) object &body body)
  (once-only (object)
    `(let (,@(loop for n in names collect `(,n (,n ,object))))
       ,@body)))