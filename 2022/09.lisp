(in-package #:advent-of-code.2022.09)

;;; Advent of code 2022: day 09
;;; see https://adventofcode.com/2022/day/9

(defun parse-line (line)
  (let ((*package* (find-package :keyword)))
    (with-input-from-string (in line)
      (list (read in) (read in)))))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun dir-delta (symbol)
  (ecase symbol
    (:L #c(-1 0))
    (:R #c(1 0))
    (:U #c(0 -1))
    (:D #c(0 1))))

(defun part-1 (&optional (data (parse-input)))
  (let ((head 0)
	(tail 0)
	(visited (make-hash-table)))
    (setf (gethash tail visited) t)
    (loop for (dir steps) in data
	  for delta = (dir-delta dir)
	  do (dotimes (i steps)
	       (incf head delta)
	       (let ((dist (- head tail)))
		 (when (or (< 1 (abs (realpart dist)))
			   (< 1 (abs (imagpart dist))))
		   ;; tail must come closer
		   (incf tail
			 (complex (signum (realpart dist))
				  (signum (imagpart dist))))
		   (setf (gethash tail visited) t)))))
    (hash-table-count visited)))

(defun part-2 (&optional (data (parse-input)))
  (let ((knots (make-array 10 :initial-element 0))
	(visited (make-hash-table)))
    (setf (gethash 0 visited) t)
    (loop for (dir steps) in data
	  for delta = (dir-delta dir)
	  do (dotimes (i steps)
	       (incf (aref knots 0) delta)
	       (dotimes (nhead 9)
		 (let ((ntail (1+ nhead)))
		   (let ((dist (- (aref knots nhead)
				  (aref knots ntail))))
		     (when (or (< 1 (abs (realpart dist)))
			       (< 1 (abs (imagpart dist))))
		       ;; tail must come closer
		       (incf (aref knots ntail)
			     (complex (signum (realpart dist))
				      (signum (imagpart dist))))))))
	       (setf (gethash (aref knots 9) visited) t)))
    (hash-table-count visited)))
