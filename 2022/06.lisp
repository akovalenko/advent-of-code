(in-package #:advent-of-code.2022.06)

;;; Advent of code 2022: day 06
;;; see https://adventofcode.com/2022/day/6

(defun parse-input ()
  (first (file-lines (my-input-path))))

(defun find-marker (string group-size)
  (let (stack)
    (loop for char across string
	  for n from 1
	  do (push char stack)
	     (when (>= n group-size)
	       (when (apply '/= (mapcar 'char-code (subseq stack 0 group-size)))
		 (return n))))))

(defun part-1 (&optional (data (parse-input)))
  (find-marker data 4))

(defun part-2 (&optional (data (parse-input)))
  (find-marker data 14))
