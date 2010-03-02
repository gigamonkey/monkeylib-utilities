(in-package :com.gigamonkeys.utilities)

;;; This file provides an API for manipulating dates (N.B. just dates,
;;; not dates and times). The API is, I hope, reasonable. The
;;; implementation is extremely brute force and brain dead. 



(defclass date ()
  ((day :initarg :day :accessor day)
   (month :initarg :month :accessor month)
   (year :initarg :year :accessor year)))

(defun date< (d1 d2)
  (< (date->utc d1) (date->utc d2)))

(defun date<= (d1 d2)
  (<= (date->utc d1) (date->utc d2)))

(defun date>= (d1 d2)
  (>= (date->utc d1) (date->utc d2)))

(defun date> (d1 d2)
  (> (date->utc d1) (date->utc d2)))

(defun date= (d1 d2)
  (= (date->utc d1) (date->utc d2)))

(defun date+ (d1 days)
  (let ((date d1))
    (loop repeat days do (setf date (tomorrow date)))
    date))

(defun date- (d1 days)
  (let ((date d1))
    (loop repeat days do (setf date (yesterday date)))
    date))

(defun today ()
  (date (get-universal-time)))

(defun yyyy-mm-dd (date)
  (with-slots (day month year) date
    (format nil "~d-~2,'0d-~2,'0d" year month day)))

(defgeneric date (value))

(defmethod date ((value t))
  (error "Don't know how to make a date out of ~s" value))

(defmethod date ((date date)) date)

(defmethod date ((string string))
  (let ((parts (mapcar #'parse-integer (split-sequence #\- string))))
    (make-instance 'date :year (pop parts) :month (pop parts) :day (pop parts))))

(defmethod date ((utc integer))
  (with-time (date month year) utc
    (make-instance 'date :day date :month month :year year)))

(defun make-date (&key 
		  (defaults (today))
		  (year (year defaults))
		  (month (month defaults))
		  (day (day defaults)))
  (make-instance 'date :year year :month month :day day))

(defmethod print-object ((object date) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-slots (day month year) object
      (format stream "~d-~2,'0d-~2,'0d" year month day))))

(defun date->utc (date)
  (with-slots (day month year) date
    (make-time :day day :month month :year year)))

(defun midnight (date)
  (with-slots (day month year) date
    (make-time :day day :month month :year year :second 0 :minute 0 :hour 0)))


(defun day-of-week (date)
  (with-slots (day month year) date
    (let ((utc (make-time :day day :month month :year year)))
      (with-time (day) utc
	(aref #(:monday :tuesday :wednesday :thursday :friday :saturday :sunday) day)))))

(defun leap-year-p (year)
  (or (zerop (mod year 400))
      (and (zerop (mod year 4))
	   (not (zerop (mod year 100))))))

(defun days-in-month (month year)
  (let ((base (aref #(31 28 31 30 31 30 31 31 30 31 30 31) (1- month))))
    (+ base (if (and (= month 2) (leap-year-p year)) 1 0))))
	
(defun tomorrow (date)
  (with-slots (day month year) date
    (let ((max-date (days-in-month month year)))
      (cond
	((<= (1+ day) max-date)
	 (make-instance 'date :day (1+ day) :month month :year year))
	((= month 12)
	 (make-instance 'date :day 1 :month 1 :year (1+ year)))
	(t
	 (make-instance 'date :day 1 :month (1+ month) :year year))))))

(defun yesterday (date)
  (with-slots (day month year) date
    (cond
      ((> day 1)
       (make-instance 'date :day (1- day) :month month :year year))
      ((> month 1)
       (make-instance 'date :day (days-in-month (1- month) year) :month (1- month) :year year))
      (t
       (make-instance 'date :day 31 :month 12 :year (1- year))))))

(defun next-year (date)
  (with-slots (day month year) date
    (make-instance 'date :day day :month month :year (1+ year))))

(defun this-year (date)
  (with-slots (day month) date
    (with-slots (year) (today)
      (make-instance 'date :day day :month month :year year))))

(defun days-between (d1 d2)
  (when (date< d2 d1)
    (rotatef d1 d2))
  (loop for date = d1 then (tomorrow date)
     while (date< date d2) count t))

(defun next-day (day-of-week)
  (loop for day = (tomorrow (today)) then (tomorrow day) when (eql day-of-week (day-of-week day)) return day))

(defun parse-time (time)
  "Parse a time of the form H:MM:SS or MM:SS into a number of seconds."
  (let ((parts (mapcar #'parse-integer (split-sequence #\: time))))
    (loop for p in (reverse parts)
	 for i from 0
	 summing (* (expt 60 i) p))))
