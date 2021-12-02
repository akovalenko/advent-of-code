(in-package #:advent-of-code.2021.01)

;;; Advent of code 2021: day 01 
;;; see https://adventofcode.com/2021/day/1

(defun parse-input ()
  (mapcar 'parse-integer (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)) (n 1))
  (loop for a in data
	and b in (nthcdr n data)
	count (< a b)))

(defun part-2 (&optional (data (parse-input)))
  (part-1 data 3))
