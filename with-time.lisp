;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.utilities)

;; Note these are the offsets as the rest of the world (e.g. ISO-8601)
;; thinks of them. The time-zone argument to DECODE-UNIVERSAL-TIME is
;; the negation of these numeric values.  The information that went
;; into this list was originally found at
;; <http://www.timeanddate.com/library/abbreviations/timezones/>
;; though any transcription errors are likely my own.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *time-zones*
  '(;; North America
    (:nst -7/2 "Newfoundland Standard Time")
    (:hnt -7/2 "Heure Normale de Terre-Neuve")
    (:ndt -5/2 "Newfoundland Daylight Time")
    (:hat -5/2 "Heure Avancée de Terre-Neuve")
    (:haa -3 "Heure Avancée de l'Atlantique")
    (:adt -3 "Atlantic Daylight Time")
    (:hna -4 "Heure Normale de l'Atlantique")
    (:hae -4 "Heure Avancée de l'Est")
    (:edt -4 "Eastern Daylight Time")
    (:ast -4 "Atlantic Standard Time")
    (:hne -5 "Heure Normale de l'Est")
    (:hac -5 "Heure Avancée du Centre")
    (:est -5 "Eastern Standard Time")
    (:cdt -5 "Central Daylight Time")
    (:mdt -6 "Mountain Daylight Time")
    (:hnc -6 "Heure Normale du Centre")
    (:har -6 "Heure Avancée des Rocheuses")
    (:cst -6 "Central Standard Time")
    (:pdt -7 "Pacific Daylight Time")
    (:mst -7 "Mountain Standard Time")
    (:hnr -7 "Heure Normale des Rocheuses")
    (:hap -7 "Heure Avancée du Pacifique")
    (:pst -8 "Pacific Standard Time")
    (:hnp -8 "Heure Normale du Pacifique")
    (:hay -8 "Heure Avancée du Yukon")
    (:akdt -8 "Alaska Daylight Time")
    (:hny -9 "Heure Normale du Yukon")
    (:hadt -9 "Hawaii-Aleutian Daylight Time")
    (:akst -9 "Alaska Standard Time")
    (:hast -10 "Hawaii-Aleutian Standard Time")
    ;; Military
    (:m 12 "Mike Time Zone")
    (:l 11 "Lima Time Zone")
    (:k 10 "Kilo Time Zone")
    (:i 9 "India Time Zone")
    (:h 8 "Hotel Time Zone")
    (:g 7 "Golf Time Zone")
    (:f 6 "Foxtrot Time Zone")
    (:e 5 "Echo Time Zone")
    (:d 4 "Delta Time Zone")
    (:c 3 "Charlie Time Zone")
    (:b 2 "Bravo Time Zone")
    (:a 1 "Alpha Time Zone")
    (:z 0 "Zulu Time Zone")
    (:n -1 "November Time Zone")
    (:o -2 "Oscar Time Zone")
    (:p -3 "Papa Time Zone")
    (:q -4 "Quebec Time Zone")
    (:r -5 "Romeo Time Zone")
    (:s -6 "Sierra Time Zone")
    (:t -7 "Tango Time Zone")
    (:u -8 "Uniform Time Zone")
    (:v -9 "Victor Time Zone")
    (:w -10 "Whiskey Time Zone")
    (:x -11 "X-ray Time Zone")
    (:y -12 "Yankee Time Zone")
    ;; Europe
    (:eest 3 "Eastern European Summer Time")
    (:mesz 2 "Mitteleuropäische Sommerzeit")
    (:eet 2 "Eastern European Time")
    (:cest 2 "Central European Summer Time")
    (:west 1 "Western European Summer Time")
    (:mez 1 "Mitteleuropäische Zeit")
    (:ist 1 "Irish Summer Time")
    (:cet 1 "Central European Time")
    (:bst 1 "British Summer Time")
    (:wet 0 "Western European Time")
    (:utc 0 "Coordinated Universal Time")
    (:gmt 0 "Greenwich Mean Time")
    ;; Australia
    (:nft 23/2 "Norfolk (Island) Time")
    (:edt 11 "Eastern Daylight Time")
    (:aedt 11 "Australian Eastern Daylight Time")
    (:cdt 21/2 "Central Daylight Time")
    (:acdt 21/2 "Australian Central Daylight Time")
    (:est 10 "Eastern Standard Time")
    (:aest 10 "Australian Eastern Standard Time")
    (:cst 19/2 "Central Standard Time")
    (:acst 19/2 "Australian Central Standard Time")
    (:wst 8 "Western Standard Time")
    (:awst 8 "Australian Western Standard Time")
    (:cxt 7 "Christmas Island Time"))))

(defparameter *long-month-names*
  #("January"
    "February"
    "March"
    "April"
    "May"
    "June"
    "July"
    "August"
    "September"
    "October"
    "November"
    "December"))

(defparameter *day-names*
  #("Monday"
    "Tuesday"
    "Wednesday"
    "Thursday"
    "Friday"
    "Saturday"
    "Sunday"))

(defun month-name (number)
  (aref *long-month-names* (1- number)))

(defun day-name (number)
  (aref *day-names* number))

(defun make-time (&rest args &key second minute hour day month year zone (defaults (get-universal-time)))
  (declare (ignore second minute hour day month year zone))
  (remf args :defaults)
  (apply #'merge-time defaults args))

;;(defun midnight (&key day month year zone)
;;  (make-time :second 0 :minute 0 :hour 0 :day day :month month :year year :zone zone))


(defun merge-time (utc &key second minute hour day month year zone)
  (multiple-value-bind (utc-second
                        utc-minute
                        utc-hour
                        utc-day
                        utc-month
                        utc-year
                        utc-day-of-week
                        utc-daylight-savings-p
                        utc-zone)
      (decode-universal-time utc)
    (declare (ignore utc-day-of-week))
    (encode-universal-time
     (or second utc-second)
     (or minute utc-minute)
     (or hour utc-hour)
     (or day utc-day)
     (or month utc-month)
     (or year utc-year)
     (or (translate-zone zone) (if utc-daylight-savings-p (1- utc-zone) utc-zone)))))

(defun translate-zone (zone)
  (typecase zone
    (number zone)
    (keyword (- (second (find zone *time-zones* :key #'first))))))

(defun reverse-translate-zone (zone)
  (first (find zone *time-zones* :key #'second)))

(defmacro with-time ((&rest args) utc &body body)
  (let* ((zone-cons (member '&zone args))
         (zone (if zone-cons (cadr zone-cons)))
         (args (nconc (ldiff args zone-cons) (cddr zone-cons)))
         (gensymed-vars ()))
    (when (find zone *time-zones* :key #'first)
      (setf zone (- (second (find zone *time-zones* :key #'first)))))
    (labels ((part-name (spec)
               (if (symbolp spec) spec (first spec)))
             (var-name (spec)
               (if (symbolp spec) spec (second spec)))
             (make-gensym () (first (push (gensym) gensymed-vars)))
             (find-var (name)
               (let ((name (find name args :key #'part-name :test #'string=)))
                 (if name (var-name name) (make-gensym)))))
      (let ((vars (mapcar #'find-var '(second minute hour date month year day daylight-p zone))))
        `(multiple-value-bind ,vars ,(if zone
                                      `(decode-universal-time ,utc ,zone)
                                      `(decode-universal-time ,utc))
           (declare (ignore ,@gensymed-vars))
           ,@body)))))

(defmacro with-current-time ((&rest args) &body body)
  `(with-time (,@args) (get-universal-time) ,@body))

;; Loosely based on code from <http://www.pvv.ntnu.no/~nsaa/ISO8601.html>
(defun format-iso-8601-time (time-value &key time-zone omit-time-zone omit-date omit-time)
  "Formats a universal time TIME-VALUE in ISO 8601 format. If no
time zone is provided the default timezone returned by
DECODE-UNIVERSAL-TIME, adjusted by daylights savings time as
appropriate, is used. The time zone information is included in
the generated string unless OMIT-TIME-ZONE is true. (In general,
if you want a shorter string, rather than omit the timezone
altogether it is better to pass an explicit time-zone argument of
0 (GMT) which will add only a single 'Z' to the string yet still
produce an unambiguous date/time string.)"
  (multiple-value-bind (second minute hour day month year day-of-week daylight-savings-p zone)
      (etypecase time-zone
        (number  (decode-universal-time time-value (- time-zone)))
        (keyword (decode-universal-time time-value (lisp-time-zone time-zone)))
        (null    (decode-universal-time time-value)))
    (declare (ignore day-of-week))
    (when daylight-savings-p (decf zone))
    (setf zone (- zone))
    (with-output-to-string (s)
      (unless omit-date
        (format s "~4,'0d-~2,'0d-~2,'0d"  year month day))
      (unless (or omit-date omit-time)
        (write-char #\T s))
      (unless omit-time
        (format s "~2,'0d:~2,'0d:~2,'0d" hour minute second)
        (unless omit-time-zone
          (if (zerop zone)
              (princ #\Z s)
              (multiple-value-bind (h m) (truncate (* 60 zone) 60)
                (format s  "~2,'0@d:~2,'0d" h (abs m)))))))))

(defpackage :iso (:use) (:export :|8601|))

(defun iso:8601 (out arg colon-p at-sign-p &rest params)
  (write-string (format-iso-8601-time
                 arg
                 :time-zone (first params)
                 :omit-time (and at-sign-p (not colon-p))
                 :omit-date colon-p
                 :omit-time-zone (and colon-p (not at-sign-p))) out))

(defun parse-iso-8601-time (text)
  ;; For now parse a full time like "2009-08-05T04:28:30Z" or
  ;; "2009-08-05T05:17:40-7:00"
  (flet ((int (start &optional end)
           (parse-integer (subseq text start end))))
    (let ((year (int 0 4))
          (month (int 5 7))
          (day (int 8 10))
          (hour (int 11 13))
          (minute (int 14 16))
          (second (int 17 19))
          (timezone (subseq text 19)))
      (cond
        ((string= timezone "Z")
         (setf timezone 0))
        (t
         (let ((colon (position #\: timezone)))
           (let ((tz-sign (if (string= (subseq timezone 0 1) "-") 1 -1))
                 (tz-hours (parse-integer (subseq timezone 1 colon)))
                 (tz-minutes (parse-integer (subseq timezone (1+ colon)))))
             (setf timezone (* tz-sign (+ tz-hours (/ tz-minutes 60))))))))
      (encode-universal-time second minute hour day month year timezone))))



(defun iso-8601-time-zone (name)
  (find name *time-zones* :key #'first))

(defun lisp-time-zone (name)
  (- (second (find name *time-zones* :key #'first))))

(defun now () (get-universal-time))

(defun date/time->utc (year month date &optional (hour 0) (minute 0) (second 0))
  (encode-universal-time second minute hour date month year))

(defun time->utc (hour minute)
  (with-time (year month date) (get-universal-time)
    (encode-universal-time 0 minute hour date month year)))

(defun hh-mm-ss (seconds)
  (multiple-value-bind (minutes seconds) (floor (round seconds) 60)
    (multiple-value-bind (hours minutes) (floor minutes 60)
      (format nil "~2,'0d:~2,'0d:~2,'0d" hours minutes seconds))))

(defun hh-mm (seconds)
  (multiple-value-bind (minutes seconds) (floor seconds 60)
    (declare (ignore seconds))
    (multiple-value-bind (hours minutes) (floor minutes 60)
      (format nil "~2,'0d:~2,'0d" hours minutes))))

(defun human-time (seconds)
  (multiple-value-bind (minutes seconds) (floor seconds 60)
    (multiple-value-bind (hours minutes) (floor minutes 60)
      (multiple-value-bind (days hours) (floor hours 24)
        (multiple-value-bind (weeks days) (floor days 7)
          (multiple-value-bind (years weeks) (floor weeks 52)
            (multiple-value-bind (centuries years) (floor years 100)
              (format nil "~[~:;~:*~d centur~:@p ~]~[~:;~:*~d year~:p ~]~[~:;~:*~d week~:p ~]~[~:;~:*~d day~:p ~]~[~:;~:*~d hour~:p ~]~[~:;~:*~d minute~:p ~]~[~:;~:*~d second~:p ~]"
                      centuries years weeks days hours minutes seconds))))))))

(defun parse-date-string (string)
  (apply #'date/time->utc
         (loop for start = 0 then (1+ dash)
            for dash = (position #\- string :start start)
            collect (parse-integer (subseq string start dash))
            while dash)))

(defun weekday-p (time)
  (with-time (day) time (< day 5)))


(defparameter *unix-epoch* (make-time :second 0 :minute 0 :hour 0 :day 1 :month 1 :year 1970 :zone :utc))

(defparameter *cocoa-epoch* (make-time :second 0 :minute 0 :hour 0 :day 1 :month 1 :year 2001 :zone :gmt))

(defun seconds-since-epoch (lisp-time epoch)
  (- lisp-time epoch))

(defun milliseconds-since-epoch (lisp-time epoch)
  (* (- lisp-time epoch) 1000))

(defun epochal-seconds->lisp (seconds epoch)
  (+ seconds epoch))

(defun epochal-milliseconds->lisp (milliseconds epoch)
  (+ (round milliseconds 1000) epoch))

(defun unix-time (&optional (utc (get-universal-time)))
  (seconds-since-epoch utc *unix-epoch*))

(defun from-unix-time (unix-seconds)
  (epochal-seconds->lisp unix-seconds *unix-epoch*))

(defun javascript-time (&optional (utc (get-universal-time)))
  "Javascript time is Unix time but in millisecond units."
  (milliseconds-since-epoch utc *unix-epoch*))

(defun from-javascript-time (javascript-time)
  "Javascript time is Unix time but in millisecond units."
  (epochal-milliseconds->lisp javascript-time *unix-epoch*))
