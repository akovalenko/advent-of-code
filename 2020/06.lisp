(in-package #:advent-of-code.2020.06)

;;; Advent of code 2020: day 06
;;; see https://adventofcode.com/2020/day/6

(defun parse-input ()
  (line-groups (file-lines (my-input-path) :remove-empty nil)))

(defconstant +alphabet-length+ 26)

(defun letter-code (char)
  (let ((digit-char-p (digit-char-p char (+ 10 +alphabet-length+))))
    (and digit-char-p (< 9 digit-char-p) (- digit-char-p 10))))

(defun answer-bits (answer)
  (let ((result (make-sequence 'bit-vector +alphabet-length+
			       :initial-element 0)))
    (loop for char across answer
	  do (setf (bit result (letter-code char)) 1)
	  finally (return result))))

(defun bits-logand (v1 &optional v2)
  (if v2 (map 'bit-vector 'logand v1 v2) v1))

(defun bits-logior (v1 &optional v2)
  (if v2 (map 'bit-vector 'logior v1 v2) v1))

(defun part-1 (&optional (data (parse-input)))
  (loop for chunk in data
	sum (count 1 (reduce 'bits-logior chunk :key #'answer-bits))))

(defun part-2 (&optional (data (parse-input)))
  (loop for chunk in data
	sum (count 1 (reduce 'bits-logand chunk :key #'answer-bits))))
