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
  (reduce '+ (subseq (sort (mapcar
			    (lambda (set)
			      (reduce '+ set))
			    data) #'>)
		     0 3)))
