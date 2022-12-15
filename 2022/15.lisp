(in-package #:advent-of-code.2022.15)

;;; Advent of code 2022: day 15
;;; see https://adventofcode.com/2022/day/15


(defun parse-line (line)
  (destructuring-bind (left right) (split-sequence #\: line)
    (list
     (complex
      (parse-integer left :start (+ 2 (search "x=" left))
			  :junk-allowed t)
      (parse-integer left :start (+ 2 (search "y=" left))
			  :junk-allowed t))
     (complex
      (parse-integer right :start (+ 2 (search "x=" right))
			   :junk-allowed t)
      (parse-integer right :start (+ 2 (search "y=" right))
			   :junk-allowed t)))))

(defun mdist (a b)
  (+ (abs (- (realpart a) (realpart b)))
     (abs (- (imagpart a) (imagpart b)))))

(defun elevation (a line)
  (abs (- (imagpart a) line)))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)) (line 2000000))
  (let ((forbidden (make-hash-table)))
    (loop for (sensor beacon) in data
	  for mdist = (mdist sensor beacon)
	  for elevation = (elevation sensor line)
	  for section-radius = (- mdist elevation)
	  for sensor-x = (realpart sensor)
	  when (<= 0 section-radius)
	    do (loop for x from (- sensor-x section-radius)
		       to (+ sensor-x section-radius)
		     do (setf (gethash x forbidden) t)))
    (loop for (sensor beacon) in data
	  for y = (imagpart beacon)
	  for x = (realpart beacon)
	  when (= y line)
	    do (remhash x forbidden))
    (hash-table-count forbidden)))

(defun part-2 (&optional (data (parse-input)) (limit 4000000))
  (flet ((good-point (point)
	   (when (and (<= 0 (realpart point) limit)
		      (<= 0 (imagpart point) limit)
		      (loop for (sensor beacon) in data
			    always (< (mdist beacon sensor)
				      (mdist point sensor))))
	     (return-from part-2 (+ (* 4000000 (realpart point))
				    (imagpart point)))))

	 (map-external-points (fn s b)
	   (let ((radius (1+ (mdist s b))))
	     (loop for i from 0 to radius
		   do (funcall fn (+ s (complex i (- radius i))))
		      (funcall fn (+ s (complex (- i) (- radius i))))
		      (funcall fn (+ s (complex i (- (- radius i)))))
		      (funcall fn (+ s (complex (- i) (- (- radius i)))))))))
    (setf data (sort data (lambda (sb1 sb2)
			    (< (apply 'mdist sb1)
			       (apply 'mdist sb2)))))
    (loop for (sensor beacon) in data
	  do (write-char #\.) 
	     (map-external-points #'good-point sensor beacon))))
