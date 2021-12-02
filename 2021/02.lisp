(in-package #:advent-of-code.2021.02)

;;; Advent of code 2021: day 02 
;;; see https://adventofcode.com/2021/day/2

(defun parse-line (line)
  (with-input-from-string (in line)
    (let ((*package* (symbol-package :keyword)))
      (list (read in) (read in)))))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)))
  (let ((depth 0)
	(x 0))
    (loop for (direction offset) in data
	  do (ecase direction
	       (:forward (incf x offset))
	       (:down (incf depth offset))
	       (:up (decf depth offset))))
    (* x depth)))

(defun part-2 (&optional (data (parse-input)))
  (let ((depth 0)
	(x 0)
	(aim 0))
    (loop for (direction offset) in data
	  do (ecase direction
	       (:forward
		(incf x offset)
		(incf depth (* aim offset)))
	       (:down (incf aim offset))
	       (:up (decf aim offset))))
    (* x depth)))
