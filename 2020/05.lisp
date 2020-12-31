(in-package #:advent-of-code.2020.05)

;;; Advent of code 2020: day 05
;;; see https://adventofcode.com/2020/day/5

(defun parse-input ()
  (file-lines (my-input-path)))

(defun seat-id (description)
  (assert (= 10 (length description)))
  (let ((row (parse-integer
	      (map 'string (lambda (char) (ecase char (#\F #\0) (#\B #\1)))
		   (subseq description 0 7)) :radix 2))
	(column (parse-integer
		 (map 'string (lambda (char)
				(ecase char (#\L #\0) (#\R #\1)))
		      (subseq description 7 10))
		 :radix 2)))
    (values (+ (* 8 row) column) row column)))


(defun part-1 (&optional (data (parse-input)))
  (reduce 'max data :key #'seat-id))

(defun part-2 (&optional (data (parse-input)))
  (let ((occupied (make-sequence 'bit-vector 1024 :initial-element 0)))
    (dolist (seat data)
      (setf (bit occupied (seat-id seat)) 1))
    (let ((first (position 1 occupied))
	  (last (position 1 occupied :from-end t)))
      (position 0 occupied :start first :end last))))

