(in-package #:advent-of-code.2022.04)

;;; Advent of code 2022: day 04
;;; see https://adventofcode.com/2022/day/4

(defun parse-line (line)
  (destructuring-bind (left right)
      (split-sequence #\, line)
    (mapcar (lambda (range)
	      (mapcar 'parse-integer (split-sequence #\- range)))
	    (list left right))))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)))
  (loop for ((a1 b1) (a2 b2)) in data
	count (or (and (<= a2 a1 b2)
		       (<= a2 b1 b2))
		  (and (<= a1 a2 b1)
		       (<= a1 b2 b1)))))

(defun part-2 (&optional (data (parse-input)))
  (loop for ((a1 b1) (a2 b2)) in data
	count (not (or (< b1 a2)
		       (< b2 a1)))))

