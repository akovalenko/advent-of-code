(in-package #:advent-of-code.2022.03)

;;; Advent of code 2022: day 03
;;; see https://adventofcode.com/2022/day/3

(defun parse-input ()
  (file-lines (my-input-path)))

(defun only (list)
  (let ((only (first list)))
    (loop for item in (rest list)
	  do (assert (eql only item)))
    only))

(defun char-prio (letter)
  (+ (if (lower-case-p letter)
	 1
	 27)
     (digit-char-p letter 36) -10))

(defun part-1 (&optional (data (parse-input)))
  (loop for line in data
	sum (char-prio
	     (only
	      (intersection
	       (coerce (subseq line 0 (/ (length line) 2)) 'list)
	       (coerce (subseq line (/ (length line) 2)) 'list))))))

(defun part-2 (&optional (data (parse-input)))
  (loop with list = data
	while list
	sum (let ((group (list (coerce (pop list) 'list)
			       (coerce (pop list) 'list)
			       (coerce (pop list) 'list))))
	      (char-prio (only (reduce 'intersection group))))))
