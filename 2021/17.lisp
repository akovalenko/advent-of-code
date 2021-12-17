(in-package #:advent-of-code.2021.17)

;;; Advent of code 2021: day 17
;;; see https://adventofcode.com/2021/day/17

(defun parse-input ()
  (let ((line (first (file-lines (my-input-path)))))
    (let ((pos-xeq (search "x=" line))
	  (pos-comma (position #\, line))
	  (pos-yeq (search "y=" line)))
      (let ((xs (subseq line (+ #.(length "x=") pos-xeq) pos-comma))
	    (ys (subseq line (+ #.(length "y=") pos-yeq))))
	(flet ((parse-range (s)
		 (let ((dots (search ".." s)))
		   (list (parse-integer s :end dots)
			 (parse-integer s :start (+ 2 dots))))))
	  (list (parse-range xs)
		(parse-range ys)))))))

(defun move (time v0)
  (let ((v-x (realpart v0))
	(v-y (imagpart v0)))
    (complex
     (let ((time (min time v-x)))
       (- (* time v-x)
	  (/ (* (signum v-x) time (1- time)) 2)))
     (- (* time v-y)
	(/ (* time (1- time)) 2)))))

(defun y-target-velocities (y)
  (nconc (loop for v from 0
	       for d0 = nil then d
	       for d = (- (expt (+ (* 2 v) 1) 2) (* 8 y))
	       until (and d0 (< (abs (* 8 y)) (abs (- d d0))))
	       when (and (<= 0 d) (= d (expt (isqrt d) 2)))
		 collect v)
	 (loop for minusv from 1
	       for v = (- minusv)
	       for d0 = nil then d
	       for d = (- (expt (+ (* 2 v) 1) 2) (* 8 y))
	       until (and d0 (< (abs (* 8 y)) (abs (- d d0))))
	       when (and (<= 0 d) (= d (expt (isqrt d) 2)))
		 collect v)))

(defun isqrt* (x)
  (let ((root (isqrt x)))
    (assert (= x (* root root)))
    root))

(defun solve-y (y v)
  (loop for fun in (list #'+ #'-)
	for time = (/ (funcall fun (1+ (* 2 v))
			       (isqrt* (- (expt (+ 1 (* 2 v)) 2)
					  (* 8 y))))
		      2)
	when (typep time '(integer 0))
	  collect time))


(defun top-height (v)
     (- (* v v)
	(/ (* v (1- v)) 2)))

(defun part-1 (&optional (data (parse-input)))
  (destructuring-bind ((x0 x1) (y0 y1)) data
    (let ((max-time
	    (loop for y from y0 to y1
		  maximize
		  (loop for v in (y-target-velocities y)
			maximize (reduce 'max (solve-y y v)))))
	  (y-velocities
	    (remove-duplicates
	     (loop for y from y0 to y1
		   nconc (y-target-velocities y)))))
      
      (loop for time from 1 to max-time
	    maximize
	    (loop for yv in y-velocities
		  maximize
		  (loop for xv from 0 to (max x0 x1)
			for point = (move time (complex xv yv))
			when (and (<= x0 (realpart point) x1)
				  (<= y0 (imagpart point) y1))
			  maximize (top-height yv)))))))

(defun part-2 (&optional (data (parse-input)))
  (destructuring-bind ((x0 x1) (y0 y1)) data
    (let ((max-time
	    (loop for y from y0 to y1
		  maximize
		  (loop for v in (y-target-velocities y)
			maximize (reduce 'max (solve-y y v)))))
	  (y-velocities
	    (remove-duplicates
	     (loop for y from y0 to y1
		   nconc (y-target-velocities y)))))
      
      (loop with table = (make-hash-table)
	    for time from 1 to max-time
	    do (loop for yv in y-velocities
		     do (loop for xv from 0 to (max x0 x1)
			      for point = (move time (complex xv yv))
			      when (and (<= x0 (realpart point) x1)
					(<= y0 (imagpart point) y1))
				do (setf (gethash (complex xv yv) table) t)))
	    finally (return (hash-table-count table))))))
