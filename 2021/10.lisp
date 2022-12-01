(in-package #:advent-of-code.2021.10)

;;; Advent of code 2021: day 10
;;; see https://adventofcode.com/2021/day/10

(defun parse-input ()
  (file-lines (my-input-path)))

(defun closing-for (open)
  (case open
    (#\( #\))
    (#\[ #\])
    (#\{ #\})
    (#\< #\>)))

(defun corruption-score (string)
  (loop with stack = nil
	for paren across string
	for matching = (closing-for paren)
	if matching
	  do (push matching stack)
	else
	  do (let ((must-be (pop stack)))
	       (unless (eql must-be paren)
		 (print paren)
		 (return-from corruption-score
		   (ecase paren
		     (#\) 3)
		     (#\] 57)
		     (#\} 1197)
		     (#\> 25137))))))
  0)

(defun part-1 (&optional (data (parse-input)))
  (reduce '+ (mapcar 'corruption-score data)))

(defun completion-score (string)
  (loop with stack = nil
	for paren across string
	for matching = (closing-for paren)
	if matching
	  do (push matching stack)
	else
	  do (let ((must-be (pop stack)))
	       (unless (eql must-be paren)
		 (return-from completion-score nil)))
	finally
	   (return
	     (let ((score 0))
	       (dolist (closing stack score)
		 (setf score
		       (+ (* score 5)
			  (or (position closing "*)]}>")
			      (error "can't happen")))))))))

(defun middle (seq)
  (elt seq (floor (length seq) 2)))

(defun part-2 (&optional (data (parse-input)))
  (middle (sort (remove nil (mapcar 'completion-score data)) #'<)))
