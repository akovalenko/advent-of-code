(in-package #:advent-of-code.2021.06)

;;; Advent of code 2021: day 06
;;; see https://adventofcode.com/2021/day/6

(defun parse-input ()
  (mapcar 'parse-integer
	  (split-sequence #\, (first (file-lines (my-input-path))))))

(defun simulate-lanternfish (list days)
  (let ((quantity (make-sequence 'simple-vector 9 :initial-element 0)))
    (dolist (n list)
      (incf (svref quantity n)))
    (flet ((forward ()
	     (let ((birthing (svref quantity 0)))
	       (loop for i from 1 to 8
		     do (setf (svref quantity (1- i))
			      (svref quantity i)))
	       (setf (svref quantity 8) birthing)
	       (incf (svref quantity 6) birthing))))
      (dotimes (i days)
	(forward)))
    (reduce '+ quantity)))

(defun part-1 (&optional (data (parse-input)))
  (simulate-lanternfish data 80))

(defun part-2 (&optional (data (parse-input)))
  (simulate-lanternfish data 256))
