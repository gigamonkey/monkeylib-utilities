(in-package :com.gigamonkeys.utilities)

(defun keywordize (s)
  "Return a keyword symbol with the name of the given string upcased."
  (intern (string-upcase s) :keyword))
