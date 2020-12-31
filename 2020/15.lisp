(in-package #:advent-of-code.2020.15)

;;; Advent of code 2020: day 15
;;; see https://adventofcode.com/2020/day/15

(defun parse-input ()
  (mapcar 'parse-integer (split-sequence #\, (first (file-lines (my-input-path))))))

(defun part-1 (&optional (data (parse-input)) (nth 2020))
  (let ((position-table (make-hash-table)))

    (loop for i from 0 and item in (subseq data 0 (1- (length data)))
	  do (setf (gethash item position-table) i))

    (let ((spoken (car (last data)))
	  (position (length data)))
      (loop for i from position below nth
	    do (let ((found (gethash spoken position-table)))
		 (setf (gethash spoken position-table) (1- position))
		 (setf spoken (if found (- position found 1) 0))
		 (incf position))
	    maximize spoken into maxval
	    finally (return spoken)))))

(defun part-2 (&optional (data (parse-input)) (nth 30000000))
  (part-1 data nth))
