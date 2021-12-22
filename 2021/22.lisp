(in-package #:advent-of-code.2021.22)

;;; Advent of code 2021: day 22
;;; see https://adventofcode.com/2021/day/22

(defun parse-line (line)
  (destructuring-bind (onoff ranges)
      (split-sequence #\Space line)
    (list (string= onoff "on")
	  (loop for range in (split-sequence #\, ranges)
		for dots = (search ".." range)
		collect (cons (parse-integer range :start 2 :end dots)
			      (parse-integer range :start (+ 2 dots)))))))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun model-small-area (data)
  (let ((bits (make-array '(101 101 101) :element-type 'bit :initial-element 0)))
    (flet ((frob-pair (cons)
	     (let ((v0 (car cons))
		   (v1 (cdr cons)))
	       (list (+ 50 (max (min v0 v1) -50))
		     (+ 50 (min (max v0 v1) 50))))))
      (loop for (turn-on (xs ys zs)) in data
	    do (loop
		 with (xa xb) = (frob-pair xs)
		 with (ya yb) = (frob-pair ys)
		 with (za zb) = (frob-pair zs)
		 for x from xa to xb
		 do (loop for y from ya to yb
			  do (loop for z from za to zb
				   do (setf (aref bits x y z)
					    (if turn-on 1 0)))))
	    finally (return (count 1 (flat-array-alias bits)))))))

(defun key-points (data axis)
  (flet ((frob-pair (cons)
	   (let ((v0 (car cons))
		 (v1 (cdr cons)))
	     (list (min v0 v1)
		   (1+ (max v0 v1))))))
    (alist-hash-table
     (loop for key in (sort
		       (remove-duplicates
			(loop for (turn-on pairs) in data
			      nconc (frob-pair (elt pairs axis))))
		       #'<)
	   for index from 0
	   collect (cons key index)))))

(defun all-key-points (data)
  (loop for axis below 3
	collect (key-points data axis)))

(defun kp-weights (kp)
  (let ((rev-kp
	  (alist-hash-table
	   (mapcar (lambda (pair)
		     (cons (cdr pair) (car pair)))
		   (hash-table-alist kp)))))
    (coerce
     (loop for index from 0
	   for this = (gethash index rev-kp)
	   and next = (gethash (1+ index) rev-kp)
	   while next
	   collect (- next this))
     'simple-vector)))

(defun model-whole-area (data)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((kp (all-key-points data))
	 (dimensions (mapcar 'hash-table-count kp))
	 (bits (make-array dimensions :element-type 'bit
				      :initial-element 0)))
    (flet ((frob-pair (cons)
	     (let ((v0 (car cons))
		   (v1 (cdr cons)))
	       (list (min v0 v1)
		     (max v0 v1)))))
      (loop for (turn-on ranges) in data
	    for ((xa . xb) (ya . yb) (za . zb))
	      = (loop for range in ranges
		      for kp-table in kp
		      for (v0 v1) = (frob-pair range)
		      collect (cons (gethash v0 kp-table)
				    (gethash (1+ v1) kp-table)))
	    do (loop with bit = (if turn-on 1 0)
		     for x from xa below xb
		     do (loop for y from ya below yb
			      do (loop for z from za below zb
				       do (setf (aref bits x y z) bit))))
	    finally
	       (return-from model-whole-area
		 (loop with (w0 w1 w2) = (mapcar 'kp-weights kp)
		       for wx across w0
		       for x from 0 below (length w0)
		       sum (* wx (loop for y from 0 below (length w1)
				       for wy across w1
				       sum (* wy (loop for z from 0
							 below (length w2)
						       for wz across w2
						       when (= 1 (aref bits x y z))
							 sum wz))))))))))

(defun part-1 (&optional (data (parse-input)))
  (model-small-area data))

(defun part-2 (&optional (data (parse-input)))
  (model-whole-area data))

