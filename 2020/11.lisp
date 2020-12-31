(in-package #:advent-of-code.2020.11)

;;; Advent of code 2020: day 11
;;; see https://adventofcode.com/2020/day/11

(defun parse-input ()
  (file-lines (my-input-path)))

(defun part-1 (&optional (data (parse-input)))
  (let* ((data (concatenate 'vector data))
	 (copy (map 'vector 'copy-seq data))
	 (width (length (aref data 0)))
	 (height (length data)))
    (labels ((dget (x y)
	       (if (and (<= 0 x (1- width))
			(<= 0 y (1- height)))
		   (aref (aref data y) x)
		   #\.))
	     (dset (x y char)
	       (setf (aref (aref copy y) x) char))
	     (forth ()
	       (dotimes (y height)
		 (dotimes (x width)
		   (unless (char= #\. (dget x y))
		     (let ((adjacent
			     (loop for dy from -1 to 1
				   sum (loop for dx from -1 to 1
					     count (and (not (= dx dy 0))
							(char= #\# (dget (+ x dx) (+ y dy))))))))
		       (cond
			 ((= 0 adjacent)
			  (dset x y #\#))
			 ((<= 4 adjacent)
			  (dset x y #\L))))))))
	     (cas ()
	       (unless (every #'string= data copy)
		 (prog1 t
		   (rotatef data copy)
		   (loop for source across data
			 and destination across copy
			 do (replace destination source))))))
      (loop
	(forth)
	;;(map nil 'write-line copy) (read-line)
	(unless (cas)
	  (return (loop for y below height
			sum (loop for x below width
				  count (char= #\# (dget x y))))))))))


(defun part-2 (&optional (data (parse-input)))
  (let* ((data (concatenate 'vector data))
	 (copy (map 'vector 'copy-seq data))
	 (width (length (aref data 0)))
	 (height (length data))
	 (neighbours (make-array (list width height 8) :initial-element t)))
    (labels ((dget (x y)
	       (if (and (<= 0 x (1- width))
			(<= 0 y (1- height)))
		   (aref (aref data y) x)
		   #\.))
	     (dset (x y char)
	       (setf (aref (aref copy y) x) char))
	     (adjacent-occupied (x y)
	       (when (eq t (aref neighbours 0 0 0))
		 (dotimes (y height)
		   (dotimes (x width)
		     (loop with nn = 0
			   for dy from -1 to 1
			   do (loop for dx from -1 to 1
				    for nx = x and ny = y
				    unless (= 0 dx dy)
				      do (loop
					   (incf nx dx)
					   (incf ny dy)
					   #- (and)
					   (format t "~&X: ~a Y: ~a NX: ~a NY: ~A NN:~A ~&"
						   x y nx ny nn)
					   (unless (and (<= 0 nx (1- width))
							(<= 0 ny (1- height)))
					     (setf (aref neighbours x y nn) nil)
					     (return))
					   (unless (char= #\. (dget nx ny))
					     ;(pprint (list :commit x y nn nx ny))
					     (setf (aref neighbours x y nn)
						   (cons nx ny))
					     (return)))
					 (incf nn)))))
		 ;(pprint neighbours)
		 ;(pprint (aref neighbours 0 0 4))
		 )
	       (loop for nn below 8
		     for nxny = (aref neighbours x y nn)
		     when nxny
		       count (char= #\# (dget (car nxny) (cdr nxny)))))
	     (forth ()
	       (dotimes (y height)
		 (dotimes (x width)
		   (unless (char= #\. (dget x y))
		     (let ((adjacent (adjacent-occupied x y)))
		       ;(pprint (list x y adjacent))
		       (cond
			 ((= 0 adjacent)
			  (dset x y #\#))
			 ((<= 5 adjacent)
			  (dset x y #\L))))))))
	     (cas ()
	       (unless (every #'string= data copy)
		 (prog1 t
		   (rotatef data copy)
		   (loop for source across data
			 and destination across copy
			 do (replace destination source))))))
      (loop
	(forth)
	;; (map nil 'write-line copy) (read-line)
	(unless (cas)
	  (return (loop for y below height
			sum (loop for x below width
				  count (char= #\# (dget x y))))))))))
