(in-package #:advent-of-code.2021.20)

;;; Advent of code 2021: day 20
;;; see https://adventofcode.com/2021/day/20

(defun parse-input ()
  (line-groups (file-lines (my-input-path) :remove-empty nil)))

(defun make-lut (string)
  (map 'simple-bit-vector (lambda (char) (ecase char (#\# 1) (#\. 0))) string))

(defun make-bits (lines &optional (pad 4))
  (let* ((w (length (first lines)))
	 (h (length lines))
	 (aw (+ w (* 2 pad)))
	 (ah (+ h (* 2 pad)))
	 (bits (make-array (list ah aw) :element-type 'bit :initial-element 0)))
    (dotimes (y h bits)
      (let ((line (pop lines)))
	(dotimes (x w)
	  (setf (aref bits (+ pad y) (+ pad x))
		(ecase (char line x)
		  (#\# 1) (#\. 0))))))))

(defun part-1 (&optional (data (parse-input)))
  (destructuring-bind (lut image) data
    (let ((lut (make-lut (format nil "~{~a~}" lut)))
	  (image (make-bits image 6))
	  (outer-matter 0))
      (flet ((key-at (y x)
	       (loop
		 with sum = 0
		 with (h w) = (array-dimensions image)
		 for dy from -1 to 1
		 do (loop for dx from -1 to 1
			  for xbit = (+ x dx)
			  and ybit = (+ y dy)
			  do (setf (ldb (byte 1 (- 8 (+ (1+ dx)
							(* 3 (1+ dy)))))
					sum)
				   (if (and (< -1 xbit w)
					    (< -1 ybit h))
				       (aref image ybit xbit)
				       outer-matter)))
		 finally (return sum))))
	(dotimes (step 2)
	  (let ((scratch (copy-array image)))
	    (loop
	      with (h w) = (array-dimensions scratch)
	      for y below h
	      do (loop for x below w
		       do (setf (aref scratch y x)
				(bit lut (key-at y x)))))
	    (setf image scratch
		  outer-matter (ecase outer-matter
				 (0 (aref lut 0))
				 (1 (aref lut 511))))))
	(count 1 (flat-array-alias image))))))

(defun part-2 (&optional (data (parse-input)))
  (destructuring-bind (lut image) data
    (let ((lut (make-lut (format nil "~{~a~}" lut)))
	  (image (make-bits image 100))
	  (outer-matter 0))
      (flet ((key-at (y x)
	       (loop
		 with sum = 0
		 with (h w) = (array-dimensions image)
		 for dy from -1 to 1
		 do (loop for dx from -1 to 1
			  for xbit = (+ x dx)
			  and ybit = (+ y dy)
			  do (setf (ldb (byte 1 (- 8 (+ (1+ dx)
							(* 3 (1+ dy)))))
					sum)
				   (if (and (< -1 xbit w)
					    (< -1 ybit h))
				       (aref image ybit xbit)
				       outer-matter)))
		 finally (return sum))))
	(dotimes (step 50)
	  (let ((scratch (copy-array image)))
	    (loop
	      with (h w) = (array-dimensions scratch)
	      for y below h
	      do (loop for x below w
		       do (setf (aref scratch y x)
				(bit lut (key-at y x)))))
	    (setf image scratch
		  outer-matter (aref lut (* 511 outer-matter)))))
	(count 1 (flat-array-alias image))))))
