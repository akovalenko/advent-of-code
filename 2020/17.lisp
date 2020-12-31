(in-package #:advent-of-code.2020.17)

;;; Advent of code 2020: day 17
;;; see https://adventofcode.com/2020/day/17

(defun parse-input ()
  (file-lines (my-input-path)))

(defun part-1 (&optional (data (parse-input)))
  (let ((width (length (first data)))
	(height (length data))
	(table (make-hash-table :test 'equal)))
    (dotimes (x width)
      (dotimes (y height)
	(when (char= #\# (char (elt data y) x))
	  (setf (gethash (list x y 0) table) t))))
    (flet ((forth ()
	     (let ((adj (make-hash-table :test 'equal))
		   (new (make-hash-table :test 'equal)))
	       (maphash (lambda (point k) k
			  (destructuring-bind (x y z) point
			    (dotimes (dx 3)
			      (dotimes (dy 3)
				(dotimes (dz 3)
				  (unless (= 1 dx dy dz)
				    (let ((p (list (+ x dx -1)
						   (+ y dy -1)
						   (+ z dz -1))))
				      (incf (gethash p adj 0)))))))))
			table)
	       (maphash (lambda (point k)
			  (case k
			    (2
			     (when (gethash point table)
			       (setf (gethash point new) t)))
			    (3
			     (setf (gethash point new) t))))
			adj)
	       (setf table new))))
      (dotimes (i 6)
	(forth))
      (hash-table-count table))))

(defun part-2 (&optional (data (parse-input)))
  (let ((width (length (first data)))
	(height (length data))
	(table (make-hash-table :test 'equal)))
    (dotimes (x width)
      (dotimes (y height)
	(when (char= #\# (char (elt data y) x))
	  (setf (gethash (list x y 0 0) table) t))))
    (flet ((forth ()
	     (let ((adj (make-hash-table :test 'equal))
		   (new (make-hash-table :test 'equal)))
	       (maphash (lambda (point k) k
			  (destructuring-bind (x y z w) point
			    (dotimes (dx 3)
			      (dotimes (dy 3)
				(dotimes (dz 3)
				  (dotimes (dw 3)
				    (unless (= 1 dx dy dz dw)
				      (let ((p (list (+ x dx -1)
						     (+ y dy -1)
						     (+ z dz -1)
						     (+ w dw -1))))
					(incf (gethash p adj 0))))))))))
			table)
	       (maphash (lambda (point k)
			  (case k
			    (2
			     (when (gethash point table)
			       (setf (gethash point new) t)))
			    (3
			     (setf (gethash point new) t))))
			adj)
	       (setf table new))))
      (dotimes (i 6)
	(forth))
      (hash-table-count table))))
