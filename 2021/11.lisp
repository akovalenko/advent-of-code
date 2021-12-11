(in-package #:advent-of-code.2021.11)

;;; Advent of code 2021: day 11
;;; see https://adventofcode.com/2021/day/11

(defun parse-line (string)
  (map 'list 'digit-char-p string))

(defun parse-input ()
  (make-array '(10 10)
	      :element-type '(unsigned-byte 8)
	      :initial-contents
	      (mapcar 'parse-line (file-lines (my-input-path)))))

(defun part-1 (&optional (data (parse-input)) (steps 100))
  (let ((count 0))
    (flet ((tick ()
	     (let ((flashing))
	       (dotimes (y 10)
		 (dotimes (x 10)
		   (when (= 10 (incf (aref data y x)))
		     (push (cons y x) flashing)
		     (incf count))))
	       (loop while flashing do
		 (loop with next-flashing = nil
		       for (y . x) in flashing do
			 (loop for dy from -1 to 1
			       do (loop for dx from -1 to 1
					unless (= dx dy 0)
					  do (let ((x1 (+ x dx))
						   (y1 (+ y dy)))
					       (when (and (< -1 x1 10)
							  (< -1 y1 10)
							  (< (aref data y1 x1) 10))
						 (when (= 10 (incf (aref data y1 x1)))
						   (push (cons y1 x1) next-flashing)
						   (incf count))))))
		       finally (setf flashing next-flashing))))
	     (dotimes (y 10)
	       (dotimes (x 10)
		 (when (< 9 (aref data y x))
		   (setf (aref data y x) 0))))))
      (dotimes (i steps) (tick))
      count)))

(defun part-2 (&optional (data (parse-input)))
  (let ((count 0))
    (flet ((tick ()
	     (let ((flashing))
	       (dotimes (y 10)
		 (dotimes (x 10)
		   (when (= 10 (incf (aref data y x)))
		     (push (cons y x) flashing)
		     (incf count))))
	       (loop while flashing do
		 (loop with next-flashing = nil
		       for (y . x) in flashing do
			 (loop for dy from -1 to 1
			       do (loop for dx from -1 to 1
					unless (= dx dy 0)
					  do (let ((x1 (+ x dx))
						   (y1 (+ y dy)))
					       (when (and (< -1 x1 10)
							  (< -1 y1 10)
							  (< (aref data y1 x1) 10))
						 (when (= 10 (incf (aref data y1 x1)))
						   (push (cons y1 x1) next-flashing)
						   (incf count))))))
		       finally (setf flashing next-flashing))))
	     (dotimes (y 10)
	       (dotimes (x 10)
		 (when (< 9 (aref data y x))
		   (setf (aref data y x) 0))))))
      (loop for i from 0 do
	(tick)
	(when (= 100 (shiftf count 0))
	  (return (1+ i)))))))
