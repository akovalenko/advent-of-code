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

;;; alternative
(defun find-marker-fast (data group-size)
  (when (<= group-size (length data))
    (let ((histogram (make-array 128 :initial-element 0))
	  (ones 0))
      (flet ((register (char)
	       (case (incf (aref histogram (char-code char)))
		 (1 (incf ones))
		 (2 (decf ones))))
	     (forget (char)
	       (case (decf (aref histogram (char-code char)))
		 (1 (incf ones))
		 (0 (decf ones)))))
	(dotimes (i group-size)
	  (register (char data i)))
	(loop
	  for start from 0
	  for end from group-size
	  do (when (= ones group-size)
	       (return end))
	     (when (= end (length data))
	       (return nil))
	     (forget (char data start))
	     (register (char data end)))))))


