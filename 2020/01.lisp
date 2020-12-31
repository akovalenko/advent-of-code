(in-package #:advent-of-code.2020.01)

;;; Advent of code 2020: day 01
;;; see https://adventofcode.com/2020/day/1

(defun parse-input ()
  (mapcar 'parse-integer (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)))
  (dolist (i data)
    (dolist (j data)
      (when (= 2020 (+ i j))
	(return-from part-1 (* i j))))))

(defun part-2 (&optional (data (parse-input)))
  (let ((bits (make-sequence 'bit-vector 2020 :initial-element 0)))
    (dolist (i data)
      (setf (aref bits i) 1))
    (dolist (i data)
      (dolist (j data)
	(let ((k (- 2020 (+ i j))))
	  (when (and (< 0 k 2020)
		     (= 1 (bit bits k)))
	    (return-from part-2 (* i j k))))))))

