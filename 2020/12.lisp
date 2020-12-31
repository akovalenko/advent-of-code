(in-package #:advent-of-code.2020.12)

;;; Advent of code 2020: day 12
;;; see https://adventofcode.com/2020/day/12

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun parse-line (line)
  (cons (char line 0) (parse-integer line :start 1)))

(defparameter *directions*
  '((#\N . #c(0 1))
    (#\S . #c(0 -1))
    (#\W . #c(-1 0))
    (#\E . #c(1 0))))

(defparameter *rotations*
  '((#\L . #c(0 1))
    (#\R . #c(0 -1))))

(defun div (divided divisor)
  (multiple-value-bind (result remainder) (floor divided divisor)
    (assert (= 0 remainder))
    result))

(defun manhattan-distance (complex)
  (+ (abs (realpart complex))
     (abs (imagpart complex))))

(defun part-1 (&optional (data (parse-input)))
  (let ((location #c(0 0))
	(facing #c(1 0)))
    (loop for (command . arg) in data
	  do (ecase command
	       ((#\N #\S #\W #\E)
		(incf location (* arg (cdr (assoc command *directions*)))))
	       ((#\L #\R)
		(setf facing (* facing
				(expt (cdr (assoc command *rotations*))
				      (div arg 90)))))
	       ((#\F)
		(incf location (* facing arg)))))
    (manhattan-distance location)))


(defun part-2 (&optional (data (parse-input)))
  (let ((location #c(0 0))
	(waypoint #c(10 1)))
    (loop for (command . arg) in data
	  do (ecase command
	       ((#\N #\S #\W #\E)
		(incf waypoint (* arg (cdr (assoc command *directions*)))))
	       ((#\L #\R)
		(setf waypoint (* waypoint
				  (expt (cdr (assoc command *rotations*))
					(div arg 90)))))
	       ((#\F)
		(incf location (* waypoint arg)))))
    (manhattan-distance location)))
