(in-package #:advent-of-code.2021.07)

;;; Advent of code 2021: day 07
;;; see https://adventofcode.com/2021/day/7

(defun parse-input ()
  (mapcar 'parse-integer
   (split-sequence
    #\, (first
	 (file-lines
	  (my-input-path))))))

(defun motion-cost (position data)
  (loop for old in data
	sum (abs (- old position))))

(defun part-1 (&optional (data (parse-input)))
  (loop with best-cost = nil
	for x from (reduce 'min data) to (reduce 'max data)
	minimize (motion-cost x data)))

(defun adjusted-motion-cost (position data)
  (loop for old in data
	for delta = (abs (- old position))
	sum (floor (+ (* delta delta) delta) 2)))

(defun part-2 (&optional (data (parse-input)))
  (loop for x from (reduce 'min data) to (reduce 'max data)
	minimize (adjusted-motion-cost x data)))
