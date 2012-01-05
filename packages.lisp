;;
;; Copyright (c) 2005-2012 Gigamonkeys Consulting All rights reserved.
;;

(in-package :cl-user)

(defpackage :com.gigamonkeys.utilities
  (:use :cl :split-sequence :alexandria)
  ;; These are also defined in alexandria. At some point should use
  ;; theirs but they aren't exactly the same so that requires finding
  ;; all the places I use them.
  (:shadow :copy-file :with-output-to-file)
  (:export
   :!
   :copy-file
   :dump-file
   :file->list
   :file->lines
   :file->sexp
   :make-heap
   :heap>
   :heap-pop
   :heap-peek
   :heap-push
   :hh-mm-ss
   :hh-mm
   :human-time
   :empty-heap
   :file-text
   :file-char-length
   :file-bytes
   :with-time
   :with-current-time
   :with-slot-values
   :with-output-to-file
   :with-data-io-syntax
   :with-data-to-file
   :with-lock-file
   :weekday-p
   :lisp-time-zone
   :iso-8601-time-zone
   :format-iso-8601-time
   :parse-iso-8601-time
   :make-time
   :merge-time
   :month-name
   :day
   :day-name
   :parse-date-string
   :reverse-translate-zone
   :random-selection
   :now
   :unix-time
   :mklist
   :unlist
   :destructuring-data
   :&zone
   :date/time->utc
   :time->utc

   ;;:make-timer
   ;;:shutdown-timer
   ;;:schedule-event
   ;;:cancel-event

   :shuffle-vector
   :shuffle-list
   :nshuffle-vector

   :date
   :make-date
   :year
   :month
   :date<
   :date<=
   :date=
   :date+
   :date-
   :date>
   :date>=
   :today
   :yyyy-mm-dd
   :date->utc
   :midnight
   :day-of-week
   :leap-year-p
   :days-in-month
   :tomorrow
   :yesterday
   :next-year
   :this-year
   :days-between
   :next-day
   :parse-time
   :round-to
   :round-to-unit-fraction
   :keywordize)

  ;; Re-export inherited symbols -- too lazy to type them all.
  (:export . #.(loop for sym being the external-symbols of :split-sequence collect sym))
  (:export . #.(loop for sym being the external-symbols of :alexandria collect sym)))
