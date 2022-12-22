(in-package #:advent-of-code.2022.20)

;;; Advent of code 2022: day 20
;;; see https://adventofcode.com/2022/day/20

(defun parse-input ()
  (mapcar 'parse-integer (file-lines (my-input-path))))


(defun part-1 (&optional (data (parse-input)))
  (let ((length (length data))
	(field (coerce (loop for i in data
			     for n from 0
			     collect (cons n i))
		       'simple-vector)))
    (loop for n below length
	  for position = (position n field :key 'car)
	  for value = (cdr (svref field position))
	  for delta = (mod value (1- length)) ;; 4
	  for moveto-a = (+ delta position)
	  for moveto = (- moveto-a (if (< -1 moveto-a length) 0 (1- length)))

	  for abs = (abs (- moveto position))
	  for signum = (signum (- moveto position))
	  do (loop with i = position
		   repeat abs
		   do (rotatef (svref field i)
			       (svref field (setf i (+ i signum))))))
    (let ((zpos (position 0 field :key 'cdr)))
      (loop for d in '(1000 2000 3000)
	    collect (cdr (svref field (mod (+ d zpos) length)))))))


(defun part-2 (&optional (data (parse-input)) (dk 811589153))
  (let ((length (length data))
	(field (coerce (loop for i in data
			     for n from 0
			     collect (cons n (* i dk)))
		       'simple-vector)))
    (dotimes (i 10)
      (loop for n below length
	    for position = (position n field :key 'car)
	    for value = (cdr (svref field position))
	    for delta = (mod value (1- length)) ;; 4
	    for moveto-a = (+ delta position)
	    for moveto = (- moveto-a (if (< -1 moveto-a length) 0 (1- length)))

	    for abs = (abs (- moveto position))
	    for signum = (signum (- moveto position))
	    do (loop with i = position
		     repeat abs
		     do (rotatef (svref field i)
				 (svref field (setf i (+ i signum)))))))
    
    (let ((zpos (position 0 field :key 'cdr)))
      (loop for d in '(1000 2000 3000)
	    collect (cdr (svref field (mod (+ d zpos) length)))))))
