;;
;; Copyright (c) 2005-2010, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.utilities
  :components
  ((:file "packages")
   (:file "heap" :depends-on ("packages"))
   (:file "clos" :depends-on ("packages"))
   (:file "date" :depends-on ("packages" "with-time"))
   (:file "files" :depends-on ("packages"))
   (:file "numbers" :depends-on ("packages"))
   (:file "randomization" :depends-on ("packages"))
   #+allegro(:file "timer" :depends-on ("packages"))
   (:file "with-time" :depends-on ("packages")))
  :depends-on (:com.gigamonkeys.macro-utilities :split-sequence))