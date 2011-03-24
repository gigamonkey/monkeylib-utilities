;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :cl-user)

(defpackage :com.gigamonkeys.utilities
  (:use :cl :com.gigamonkeys.macro-utilities :split-sequence)
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

   ;; Re-export split-sequence functions
   :split-sequence
   :split-sequence-if
   :split-sequence-if-not))