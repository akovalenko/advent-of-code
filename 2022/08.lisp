(in-package #:advent-of-code.2022.08)

;;; Advent of code 2022: day 08
;;; see https://adventofcode.com/2022/day/8

(defun parse-input ()
  (map 'vector 'identity (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)))
  (let* ((w (length (elt data 0)))
	 (h (length data))
	 (vmap (make-array (list w h) :initial-element 0)))
    (flet ((mark-visibility (mask x y dx dy)
	     (loop
	       with max-height = -1
	       do (when (> (digit-char-p (elt (elt data y) x))
			   max-height)
		    (setf (aref vmap y x) (logior (aref vmap y x) mask)
			  max-height (digit-char-p (elt (elt data y) x))))
		  (let ((x1 (+ x dx))
			(y1 (+ y dy)))
		    (unless (and (< -1 x1 w)
				 (< -1 y1 h))
		      (return))
		    (setf x x1 y y1)))))

      (loop for x below w
	    do (mark-visibility 1 x 0 0 1)
	       (mark-visibility 2 x (1- h) 0 -1))

      (loop for y below h
	    do (mark-visibility 4 0 y 1 0)
	       (mark-visibility 8 (1- w) y -1 0))

      (loop for y below h
	    sum (loop for x below w
		      count (not (zerop (aref vmap y x))))))))

(defun part-2 (&optional (data (parse-input)))
  (let* ((data (map 'vector (lambda (row)
			      (map 'vector 'digit-char-p row))
		    data))
	 (w (length (elt data 0)))
	 (h (length data))
	 (score (make-array (list h w))))
    (flet ((count-visibility (x y dx dy)
	     (let ((height (aref (aref data y) x))
		   (seen 0))
	       (loop
		 (incf x dx)
		 (incf y dy)
		 (unless (and (< -1 x w)
			      (< -1 y h))
		   (return seen))
		 (incf seen)
		 (when (<= height
			   (aref (aref data y) x))
		   (return seen))))))
      (loop for y below h
	    do (loop for x below w
		     do (setf (aref score y x)
			      (* (count-visibility x y 1 0)
				 (count-visibility x y 0 1)
				 (count-visibility x y 0 -1)
				 (count-visibility x y -1 0)))))
      (loop for y below h
	    maximize (loop for x below w
			   maximize (aref score y x))))))

