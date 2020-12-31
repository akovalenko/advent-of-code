(in-package #:advent-of-code.2020.03)

;;; Advent of code 2020: day 03
;;; see https://adventofcode.com/2020/day/3

(defun parse-input ()
  (concatenate 'vector (file-lines (my-input-path))))

(defun test-slope (data right down)
  (let ((width (length (elt data 0)))
	(height (length data)))
    (loop with x = 0 and y = 0
	  while (< y height)
	  count (char= #\# (char (elt data y) (mod x width)))
	  do (incf x right)
	     (incf y down))))

(defun part-1 (&optional (data (parse-input)))
  (test-slope data 3 1))

(defun part-2 (&optional (data (parse-input)))
  (reduce '* (loop for (right down) in '((1 1) (3 1) (5 1) (7 1) (1 2))
		   collect (test-slope data right down))))
