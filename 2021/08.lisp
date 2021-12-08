(in-package #:advent-of-code.2021.08)

;;; Advent of code 2021: day 08
;;; see https://adventofcode.com/2021/day/8

(defun parse-line (line)
  (loop for part in (split-sequence #\| line)
	collect (split-sequence  #\Space part :remove-empty-subseqs t)))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)))
  (let ((unique-counts '(2 4 3 7)))
    (loop for (patterns seen) in data
	  sum (loop for item in seen
		    count (member (length item) unique-counts)))))

(defun letter-digit (letter)
  (- (digit-char-p letter 36) 10))

(defun letter-mask (letter)
  (ash 1 (letter-digit letter)))

(defun combo-mask (string)
  (reduce 'logior string :key 'letter-mask))

(defparameter *digit-masks*
  (map 'simple-vector
       'combo-mask
       '("abcefg" "cf" "acdeg" "acdfg" "bcdf"
	 "abdfg" "abdefg" "acf" "abcdefg" "abcdfg")))

(defparameter *digit-values*
  (let ((bits (make-sequence 'simple-vector 128 :initial-element nil)))
    (loop for mask across *digit-masks*
	  for digit from 0
	  do (setf (aref bits mask) digit))
    bits))

(defun make-permutation-table ()
  (let ((table (make-sequence 'simple-vector 5040))
	(index 0))
    (map-permutations
     (lambda (permutation)
       (let ((entry (make-array 128 :element-type '(unsigned-byte 7))))
	 (loop for i below 128
	       do (setf (aref entry i)
			(loop for bit below 7
			      when (logbitp bit i)
				sum (ash 1 (aref permutation bit)))))
	 (setf (aref table index) entry
	       index (1+ index))))
     #(0 1 2 3 4 5 6))
    table))

(defparameter *permutation-tables* (make-permutation-table))

(defun find-decoder (masks)
  (loop for pt across *permutation-tables*
	when (loop for mask in masks
		   always (aref *digit-values* (aref pt mask)))
	  return pt))

(defun decode-pair (patterns seen)
  (let ((pt (find-decoder
	     (mapcar 'combo-mask patterns))))
    (parse-integer
     (with-output-to-string (output)
       (dolist (s seen)
	 (write-char (digit-char (aref *digit-values* (aref pt (combo-mask s))))
		     output))))))

(defun part-2 (&optional (data (parse-input)))
  (loop for (patterns seen) in data
	sum (decode-pair patterns seen)))
