(in-package #:advent-of-code.2021.12)

;;; Advent of code 2021: day 12
;;; see https://adventofcode.com/2021/day/12

(defun parse-line (string)
  (split-sequence #\- string))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun next-points (data from)
  (loop for (a b) in data
	when (string= from a)
	  collect b
	when (string= from b)
	  collect a))

(defun path-count (data start avoid)
  (if (string= start "end")
      1
      (let ((avoid (if (upper-case-p (char start 0))
		       avoid (cons start avoid))))
	(loop for next in (next-points data start)
	      unless (member next avoid :test 'string=)
		sum (path-count data next avoid)))))

(defun path-count-p2 (data start avoid revisited)
  (if (string= start "end")
      1
      (let ((avoid (if (upper-case-p (char start 0))
		       avoid (cons start avoid))))
	(if revisited
	    (loop for next in (next-points data start)
		  unless (member next avoid :test 'string=)
		    sum (path-count-p2 data next avoid revisited))
	    (loop for next in (next-points data start)
		  unless (string= next "start")
		    sum (path-count-p2 data next avoid
				       (member next avoid :test 'string=)))))))

(defun part-1 (&optional (data (parse-input)))
  (path-count data "start" nil))

(defun part-2 (&optional (data (parse-input)))
  (path-count-p2 data "start" nil nil))
