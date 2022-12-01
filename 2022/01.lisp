(in-package #:advent-of-code.2022.01)

;;; Advent of code 2022: day 01
;;; see https://adventofcode.com/2022/day/1

(defun parse-input ()
  (mapcar
   (lambda (set)
     (mapcar 'parse-integer set))
   (remove
    nil
    (split-sequence
     ""
     (file-lines (my-input-path) :remove-empty nil)
     :test 'string=))))


(defun part-1 (&optional (data (parse-input)))
  (reduce 'max
	  (mapcar
	   (lambda (set)
	     (reduce '+ set))
	   data)))


(defun part-2 (&optional (data (parse-input)))
  (let ((queue (make-hpqueue :predicate #'>)))
    (loop for item in (mapcar
		       (lambda (set)
			 (reduce '+ set))
		       data)
	  do (hpqueue-push item item queue))
    (loop for n below 3
	  sum (hpqueue-pop queue))))
