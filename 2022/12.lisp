(in-package #:advent-of-code.2022.12)

;;; Advent of code 2022: day 12
;;; see https://adventofcode.com/2022/day/12

(defun parse-input ()
  (let* ((lines (file-lines (my-input-path)))
	 (width (length (first lines)))
	 (height (length lines))
	 (grid (make-array (list height width)))
	 (start nil)
	 (end nil))
    (loop for y below height
	  for line in lines
	  do (loop for x below width
		   for char = (char line x)
		   do (case char
			(#\S (setf start (complex y x)))
			(#\E (setf end (complex y x)))
			(otherwise
			 (setf (aref grid y x)
			       (- (digit-char-p char 36) 10))))))
    (setf (caref grid start) 0
	  (caref grid end) 25)
    (list grid start end)))


(defun part-1 (&optional (data (parse-input)))
  (destructuring-bind (grid start end) data
    (destructuring-bind (height width) (array-dimensions grid)
      (flet ((neighbours (cell)
	       (loop for delta in '(#c (1 0) #c (-1 0) #c (0 1) #c (0 -1))
		     for neigh = (+ cell delta)
		     when (and (< -1 (realpart neigh) height)
			       (< -1 (imagpart neigh) width)
			       (<= (- (caref grid neigh)
				      (caref grid cell))
				   1))
		       collect neigh)))
	(let ((distances (make-hash-table))
	      (front (list start))
	      (next nil))
	  (setf (gethash start distances) 0)
	  (loop
	    (dolist (cell front)
	      (dolist (neigh (neighbours cell))
		(unless (gethash neigh distances)
		  (setf (gethash neigh distances)
			(1+ (gethash cell distances)))
		  (push neigh next))))

	    (shiftf front next nil)
	    (let ((result (gethash end distances)))
	      (when result (return result)))))))))

(defun part-2 (&optional (data (parse-input)))
  (destructuring-bind (grid start end) data
    (destructuring-bind (height width) (array-dimensions grid)
      (flet ((neighbours (cell)
	       (loop for delta in '(#c (1 0) #c (-1 0) #c (0 1) #c (0 -1))
		     for neigh = (+ cell delta)
		     when (and (< -1 (realpart neigh) height)
			       (< -1 (imagpart neigh) width)
			       (<= (- (caref grid cell)
				      (caref grid neigh))
				   1))
		       collect neigh)))
	(let ((distances (make-hash-table))
	      (front (list end))
	      (next nil))
	  (setf (gethash end distances) 0)
	  (loop
	    (dolist (cell front)
	      (dolist (neigh (neighbours cell))
		(unless (gethash neigh distances)
		  (when (= 0 (caref grid neigh))
		    (return-from part-2 (1+ (gethash cell distances))))
		  (setf (gethash neigh distances)
			(1+ (gethash cell distances)))
		  (push neigh next))))
	    (shiftf front next nil)))))))
