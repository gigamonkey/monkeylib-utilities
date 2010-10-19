(in-package :cl-user)

(defun mklist (x)
  "`x' if it is a list, otherwise a singleton list containing `x'."
  (if (listp x) x (list x)))

(defun unlist (x)
  "If `x' is a singleton list, the single item; otherwise `x'."
  (if (not (rest x)) (first x) x))

(defun run-length-encoding (list &key (test 'eql) (key 'identity))
  "Return a list of (item . count) cons cells representing the run-length encoding of the input list."
  (loop with count = 1
     for (first . rest) on list
     when (or (not rest) (not (funcall test (funcall key first) (funcall key (first rest))))) collect (cons first count) and do (setf count 1)
     else do (incf count)))
     