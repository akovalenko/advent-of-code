(in-package #:advent-of-code.2020.02)

;;; Advent of code 2020: day 02
;;; see https://adventofcode.com/2020/day/2

(defun parse-input ()
  (mapcar #'parse-line (file-lines (my-input-path))))

(defun parse-line (line)
  (destructuring-bind (rule password) (split-sequence #\: line)
    (destructuring-bind (range one-char) (split-sequence #\Space rule)
      (assert (= 1 (length one-char)))
      (destructuring-bind (first last) (split-sequence #\- range)
	(list (parse-integer first)
	      (parse-integer last)
	      (char one-char 0)
	      (subseq password 1))))))

(defun part-1 (&optional (data (parse-input)))
  (loop for (min-times max-times char password) in data
	count (<= min-times (count char password) max-times)))

(defun part-2 (&optional (data (parse-input)))
  (loop for (pos1 pos2 char password) in data
	count (xor (char= char (char password (1- pos1)))
		   (char= char (char password (1- pos2))))))

