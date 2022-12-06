(in-package #:advent-of-code.2022.05)

;;; Advent of code 2022: day 05
;;; see https://adventofcode.com/2022/day/5

(defun parse-input ()
  (let ((lines (file-lines (my-input-path)))
	(stacks (make-array 9 :initial-element nil)))
    (loop for line = (pop lines)
	  while (position #\[ line)
	  do (loop for n below (length stacks)
		   for position = (1+ (* 4 n))
		   when (< position (length line))
		     do (let ((char (char line position)))
			  (unless (eql char #\Space)
			    (push char (aref stacks n))))))
    (list (map 'vector 'reverse stacks)
	  (let ((*package* (find-package :keyword)))
	   (loop
	     for line = (pop lines)
	     while line
	       collect (with-input-from-string (in line)
			 (read in)
			 (let ((how-many (read in)))
			   (read in)
			   (let ((from (read in)))
			     (read in) 
			     (let ((to (read in)))
			       (list how-many from to))))))))))

(defun part-1 (&optional (data (parse-input)))
  (destructuring-bind (stacks moves) data
    (loop for (what from to) in moves
	  do (loop repeat what
		   do (push (pop (aref stacks (1- from)))
			    (aref stacks (1- to)))))
    (map 'string 'first (remove nil stacks))))

(defun part-2 (&optional (data (parse-input)))
  (destructuring-bind (stacks moves) data
    (loop for (what from to) in moves
	  do (unless (eql from to)
	       (loop for item in (reverse (subseq (aref stacks (1- from)) 0 what))
			      do (push item (aref stacks (1- to))))
	       (setf (aref stacks (1- from))
		     (subseq (aref stacks (1- from)) what))))
    (map 'string 'first (remove nil stacks))))
