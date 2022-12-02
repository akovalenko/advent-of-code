(in-package #:advent-of-code.2022.02)

;;; Advent of code 2022: day 02
;;; see https://adventofcode.com/2022/day/2

;;; encode 0-rock, 1-paper, 2-scissors
(defun winner-for (value)
  (mod (1+ value) 3))

(defun loser-for (value)
  (mod (1- value) 3))

(defun draw-for (value)
  value)

(defun parse-input ()
  (mapcar (lambda (line)
	    (list (ecase (char line 0)
		    (#\A 0)
		    (#\B 1)
		    (#\C 2))
		  (ecase (char line 2)
		    (#\X 0)
		    (#\Y 1)
		    (#\Z 2))))
	  (file-lines (my-input-path))))



(defun part-1 (&optional (data (parse-input)))
  (let ((outcomes (make-array '(3 3))))
    (loop for x below 3
	  do (setf (aref outcomes x (draw-for x)) 3
		   (aref outcomes x (winner-for x)) 6
		   (aref outcomes x (loser-for x)) 0))
    (loop for (opponents mine) in data
	  sum (1+ mine)
	  sum (aref outcomes opponents mine))))

(defun part-2 (&optional (data (parse-input)))
  (let ((motions (make-array '(3 3))))
    (loop for x below 3
	  do (setf (aref motions x 1)
		   (draw-for x)
		   (aref motions x 0)
		   (loser-for x)
		   (aref motions x 2)
		   (winner-for x)))
    (loop for (opponents result) in data
	  sum (let ((mine (aref motions opponents result)))
		(+ (1+ mine)
		   (* result 3))))))
