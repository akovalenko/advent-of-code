(in-package #:advent-of-code.2021.05)

;;; Advent of code 2021: day 05
;;; see https://adventofcode.com/2021/day/5

(defun parse-point (point-string)
  (mapcar 'parse-integer (split-sequence #\, point-string)))

(defun parse-line (line)
  (let* ((sep " -> ")
	 (d (or (search sep line) (error "No separator")))
	 (first (subseq line 0 d))
	 (second (subseq line (+ d (length sep)))))
    (list (parse-point first)
	  (parse-point second))))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)))
  (loop for ((x1 y1) (x2 y2)) in data
	maximize x1 into max-x
	maximize x2 into max-x
	maximize y1 into max-y
	maximize y2 into max-y
	finally
	   (return
	     (loop with map = (make-array (list (1+ max-y) (1+ max-x))
					  :initial-element 0)
		   for ((x1 y1) (x2 y2)) in data
		   do (when (= x1 x2)
			(loop for y from (min y1 y2) to (max y1 y2)
			      do (incf (aref map y x1))))
		      (when (= y1 y2)
			(loop for x from (min x1 x2) to (max x1 x2)
			      do (incf (aref map y1 x))))
		   finally
		      (return
			(loop for y below (1+ max-y)
			      sum (loop for x below (1+ max-x)
					count (<= 2 (aref map y x)))))))))

(defun part-2 (&optional (data (parse-input)))
  (loop for ((x1 y1) (x2 y2)) in data
	maximize x1 into max-x
	maximize x2 into max-x
	maximize y1 into max-y
	maximize y2 into max-y
	finally
	   (return
	     (loop with map = (make-array (list (1+ max-y) (1+ max-x))
					  :initial-element 0)
		   for ((x1 y1) (x2 y2)) in data
		   for dx = (signum (- x2 x1))
		   and dy = (signum (- y2 y1))
		   do (loop with x = x1 and y = y1
			    do (incf (aref map y x))
			       (when (and (= x x2) (= y y2))
				 (return))
			       (incf x dx)
			       (incf y dy))
		   finally
		      (return
			(loop for y below (1+ max-y)
			      sum (loop for x below (1+ max-x)
					count (<= 2 (aref map y x)))))))))
