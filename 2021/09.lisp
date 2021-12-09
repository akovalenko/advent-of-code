(in-package #:advent-of-code.2021.09)

;;; Advent of code 2021: day 09
;;; see https://adventofcode.com/2021/day/9

(defun parse-line (string)
  (map 'list 'digit-char-p string))

(defun parse-input ()
  (let ((list (mapcar 'parse-line (file-lines (my-input-path)))))
    (make-array (list (length list) (length (first list)))
		:element-type '(unsigned-byte 4)
		:initial-contents list)))

(defun part-1 (&optional (data (parse-input)))
  (destructuring-bind (h w) (array-dimensions data)
    (labels ((at (y x)
	       (if (and (< -1 y h)
			(< -1 x w))
		   (aref data y x) 10))
	     (lowpointp (y x)
	       (let ((value (at y x)))
		 (and (< value (at y (1- x)))
		      (< value (at y (1+ x)))
		      (< value (at (1- y) x))
		      (< value (at (1+ y) x))))))
      (loop for y below h
	    sum (loop for x below w
		      when (lowpointp y x)
			sum (1+ (at y x)))))))


(defun low-points (data)
  (destructuring-bind (h w) (array-dimensions data)
    (labels ((at (y x)
	       (if (and (< -1 y h)
			(< -1 x w))
		   (aref data y x) 10))
	     (lowpointp (y x)
	       (let ((value (at y x)))
		 (and (< value (at y (1- x)))
		      (< value (at y (1+ x)))
		      (< value (at (1- y) x))
		      (< value (at (1+ y) x))))))
      (loop for y below h
	    nconc (loop for x below w
			when (lowpointp y x)
			  collect (list y x))))))

(defun part-2 (&optional (data (parse-input)))
  (let ((low-points (low-points data)))
    (destructuring-bind (h w) (array-dimensions data)
      (let ((visited (make-array (list h w) :initial-element nil))
	    (count (make-hash-table))
	    (deltas '((0 1) (0 -1) (-1 0) (1 0))))
	(labels ((at (y x)
		   (if (and (< -1 y h)
			    (< -1 x w))
		       (aref data y x) 10))
		 (paint (y x code)
		   (assert (not (aref visited y x)))
		   (let ((front (list (list y x))))
		     (incf (gethash code count 0))
		     (setf (aref visited y x) t)
		     (loop while front
			   do (print front)
			      (setf front
				    (loop for (y0 x0) in front
					  nconc
					  (loop for (dy dx) in deltas
						for y1 = (+ y0 dy)
						and x1 = (+ x0 dx)
						when (and
						      (< (at y0 x0) (at y1 x1) 9)
						      (not (aref visited y1 x1)))
						  collect (list y1 x1)
						  and do (setf (aref visited y1 x1)
							       t)
							 (incf (gethash code count 0)))
					  ))))))
	  
	  (loop for (y x) in low-points
		for id from 0
		do (paint y x id))
	  (reduce '* (subseq (sort (hash-table-values count) #'> ) 0 3)))))))
