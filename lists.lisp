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

(defmacro destructuring-data ((&rest tree) thing &body body)
  "Destructure a list as with DESTRUCTURING-BIND except anywhere in
  the pattern that a keyword appears, the same keyword must appear in
  the data."
  (multiple-value-bind (pattern keywords) (destructuring-pattern tree)
    `(destructuring-bind (,@pattern) ,thing
       ,@(loop for (var . value) in keywords collect
              `(unless (eql ,var ,value)
                 (error "Expected ~a; got ~a" ,value ,var)))
       ,@body)))

(defun destructuring-pattern (pattern)
  (let ((keywords ()))
    (labels ((walk (x)
               (typecase x
                 (keyword
                  (let ((var (gensym)))
                    (push (cons var x) keywords)
                    var))
                 (symbol x)
                 (cons (cons (walk (car x)) (walk (cdr x)))))))
      (values (walk pattern) keywords))))
