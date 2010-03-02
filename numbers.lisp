(in-package :com.gigamonkeys.utilities)

(defun round-to (number n &optional (rounding-function #'round))
  "Round to nearest `n'. (round-to x 1) === (round x)"
  (* (funcall rounding-function (/ number n)) n))

(defun round-unit-fraction (ratio)
  "Return unit fraction approximation to `ratio'."
  ;; This function, though elegant, doesn't necessarily return the
  ;; closest unit fraction. According to Jafet on #lisp it "Returns
  ;; the unit fraction 1/n for input x where x is between the harmonic
  ;; mean of 1/n and 1/(n-1) and the harmonic mean of 1/n and 1/(n+1)"
  (/ (round (/ ratio))))

(defun closest-unit-fraction (ratio)
  "Return nearest unit fraction to `ratio'."
  (let ((f (/ (floor (/ ratio))))
	(c (/ (ceiling (/ ratio)))))
    (if (< (abs (- f ratio)) (abs (- c ratio))) f c)))

(defun ceiling-unit-fraction (ratio)
  "Return nearest unit fraction greater than `ratio'."
  (/ (floor (/ ratio))))

(defun floor-unit-fraction (ratio)
  "Return nearest unit fraction less than `ratio'."
  (/ (ceiling (/ ratio))))

(defun truncate-unit-fraction (ratio)
  "Return nearest unit fraction closer to zero than `ratio'."
  (if (plusp ratio)
      (floor-unit-fraction ratio)
      (ceiling-unit-fraction ratio)))

(defun reverse-truncate-unit-fraction (ratio)
  "Return nearest unit fraction farther from zero than `ratio'."
  (if (plusp ratio)
      (ceiling-unit-fraction ratio)
      (floor-unit-fraction ratio)))
