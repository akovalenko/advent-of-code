(in-package #:advent-of-code.2022.14)

;;; Advent of code 2022: day 14
;;; see https://adventofcode.com/2022/day/14

(defun parse-line (line)
  (let ((pairs  (split-sequence #\> line)))
    (loop
      for pair in pairs
      for (left right) = (split-sequence #\, pair)
      collect (complex (parse-integer left)
		       (parse-integer right :junk-allowed t)))))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)))
  (let ((field (make-hash-table))
	(height 0))
    (loop for seq in data
	  do (loop for (this next) on seq
		   when (< height (imagpart this))
		     do (setf height (imagpart this))
		   when next
		     do (let ((delta (complex (signum (- (realpart next) (realpart this)))
					      (signum (- (imagpart next) (imagpart this))))))
			  (loop (setf (gethash this field) :rock)
				(when (= this next)
				  (return))
				(incf this delta)))))
    (flet ((sand-stationary-p ()
	     (let ((sand 500))
	       (loop
		 (when (= height (imagpart sand))
		   (return nil))
		 (let ((moved nil))
		   (loop for delta in '(#c(0 1) #c(-1 1) #c (1 1))
			 unless (gethash (+ sand delta) field)
			   do (incf sand delta)
			      (setf moved t)
			      (return))
		   (unless moved
		     (return sand)))))))
      (loop for rested from 0
	    for rest-at = (sand-stationary-p)
	    unless rest-at
	      do (return rested)
	    do (setf (gethash rest-at field) :sand)))))

(defun part-2 (&optional (data (parse-input)))
  (let ((field (make-hash-table))
	(height 0))
    (loop for seq in data
	  do (loop for (this next) on seq
		   when (< height (imagpart this))
		     do (setf height (imagpart this))
		   when next
		     do (let ((delta (complex (signum (- (realpart next) (realpart this)))
					      (signum (- (imagpart next) (imagpart this))))))
			  (loop (setf (gethash this field) :rock)
				(when (= this next)
				  (return))
				(incf this delta)))))
    (flet ((sand-stationary ()
	     (let ((sand 500))
	       (loop
		 (when (= (1+ height) (imagpart sand))
		   (return sand))
		 (let ((moved nil))
		   (loop for delta in '(#c(0 1) #c(-1 1) #c (1 1))
			 unless (gethash (+ sand delta) field)
			   do (incf sand delta)
			      (setf moved t)
			      (return))
		   (unless moved
		     (return sand)))))))
      (loop for rested from 0
	    for rest-at = (sand-stationary)
	    when (= rest-at 500)
	      do (return (1+ rested))
	    do (setf (gethash rest-at field) :sand)))))

