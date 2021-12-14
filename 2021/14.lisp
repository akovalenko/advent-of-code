(in-package #:advent-of-code.2021.14)

;;; Advent of code 2021: day 14
;;; see https://adventofcode.com/2021/day/14

(defun parse-mapping (string)
  (cons (subseq string 0 2)
	(char string 6)))

(defun parse-input ()
  (destructuring-bind ((initial) replacements)
      (line-groups (file-lines (my-input-path) :remove-empty nil))
    (list initial (alist-hash-table
		   (mapcar 'parse-mapping replacements)
		   :test 'equal))))

(defun apply-replacement (initial hash)
  (with-output-to-string (out)
    (loop for i from 0 below (1- (length initial))
	  for new = (gethash (subseq initial i (+ i 2)) hash)
	  do (write-char (char initial i) out)
	     (when new (write-char new out))
	  finally (write-char (char initial (1- (length initial))) out))))

(defun part-1 (&optional (data (parse-input)))
  (destructuring-bind (polymer rules) data
    (dotimes (i 10)
      (setf polymer (apply-replacement polymer rules)))
    (loop with hash = (make-hash-table)
	  for char across polymer
	  do (incf (gethash char hash 0))
	  finally (let ((counts (hash-table-values hash)))
		    (return (- (reduce 'max counts)
			       (reduce 'min counts)))))))

(defun letter-id (letter)
  (- (digit-char-p letter 36) 9))

(defun make-table ()
  (make-array '(27 27) :initial-element 0))

(defun rules-table (rules)
  (let ((table (make-table)))
    (maphash (lambda (key value)
	       (destructuring-bind (a b)
		   (mapcar 'letter-id (coerce key 'list))
		 (setf (aref table a b)
		       (letter-id value))))
	     rules)
    table))

(defun polymer-occur-table (polymer)
  (let ((table (make-table)))
    (loop for i below (1- (length polymer))
	  for (a b) = (mapcar 'letter-id (coerce (subseq polymer i (+ 2 i)) 'list))
	  do (incf (aref table a b)))
    (incf (aref table (letter-id (char polymer (1- (length polymer))))
		0))
    table))

(defun extend-occur-table (table rules)
  (let ((next (make-table)))
    (dotimes (a 27)
      (dotimes (b 27)
	(let ((insert (aref rules a b))
	      (count (aref table a b)))
	  (if (= 0 insert)
	      (incf (aref next a b) count)
	      (progn
		(incf (aref next a insert) count)
		(incf (aref next insert b) count))))))
    next))

(defun part-2 (&optional (data (parse-input)) (steps 40))
  (destructuring-bind (polymer rules) data
    (let ((polymer (polymer-occur-table polymer))
	  (rules (rules-table rules)))
      (dotimes (i steps)
	(setf polymer (extend-occur-table polymer rules)))
      (let ((stat
	      (loop for a from 1 to 26
		    for sum = (loop for b from 0 to 26
				    sum (aref polymer a b))
		    when (> sum 0)
		      collect sum)))
	(- (reduce 'max stat) (reduce 'min stat))))))
