(in-package #:advent-of-code.2021.03)

;;; Advent of code 2021: day 03
;;; see https://adventofcode.com/2021/day/3

(defun parse-input ()
  (file-lines (my-input-path)))

(defun most-common-bits (data)
  (let* ((count (length data))
	 (ones (make-sequence 'simple-vector (length (first data)))))
    (dolist (item data)
      (loop for i from 0
	    for digit across item
	    when (char= #\1 digit)
	      do (incf (svref ones i))))
    (let* ((gamma-rate
	     (parse-integer
	      (coerce
	       (loop for i below (length ones)
		     for nones across ones
		     collect (if (< nones (- count nones))
				 #\0
				 (if (< (- count nones) nones)
				     #\1
				     (error "Parity"))))
	       'string)
	      :radix 2))
	   (epsilon-rate
	     (- (ash 1 (length ones)) 1 gamma-rate)))
      (* gamma-rate epsilon-rate))))

(defun part-1 (&optional (data (parse-input)))
  (most-common-bits data))

(defun filter-rating (list position minority-p)
  (let* ((ones (loop for item in list
		     count (char= #\1 (char item position))))
	 (zeroes (- (length list) ones))
	 (majority
	   (if (< ones zeroes) #\0
	       (if (< zeroes ones) #\1
		   #\1)))
	 (minority (case majority (#\0 #\1) (#\1 #\0)))
	 (selector (if minority-p minority majority)))
    (loop for item in list
	  when (char= selector (char item position))
	    collect item)))

(defun part-2 (&optional (data (parse-input)))
  (let ((oxygen-rating-list data)
	(co2-rating-list data))
    (loop for bit from 0
	  while (cdr oxygen-rating-list)
	  do (setf oxygen-rating-list
		   (filter-rating oxygen-rating-list bit nil)))
    (loop for bit from 0
	  while (cdr co2-rating-list)
	  do (setf co2-rating-list
		   (filter-rating co2-rating-list bit t)))
    (* (parse-integer (first oxygen-rating-list) :radix 2)
       (parse-integer (first co2-rating-list) :radix 2))))
