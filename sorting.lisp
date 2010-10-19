(in-package :com.gigamonkeys.utilities)

(defun keyed-sort (sequence predicate key)
  "Sort `sequence' using `predicate' applied to the result of applying
  `key' to each item, using the Schwarzian Transform so `key' is only
  invoked once per item, rather than at each comparison."
  (mapcar #'cdr
          (sort
           (mapcar (lambda (x) (cons (funcall key x) x)) sequence)
           predicate :key #'car)))
  
