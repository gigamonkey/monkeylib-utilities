(in-package :com.gigamonkeys.utilities)

(defun parse-roman-numeral (symbol-or-string)
  (let ((roman-numerals 
	 (load-time-value 
	  (let ((h (make-hash-table :test #'equalp)))
	    (loop for num from 1  until (or (not (ignore-errors (format nil "~@r" num)))
					    (string= (format nil "~@r" num) (format nil "~d" num)))
	       do
		 (setf (gethash (format nil "~@r" num) h) num)
		 (setf (gethash (format nil "~:@r" num) h) num))
	    h))))
    (or (gethash (string symbol-or-string) roman-numerals)
	(error "Don't know how to parse ~a as a Roman numeral." symbol-or-string))))
