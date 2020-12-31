(in-package #:advent-of-code.2020.14)

;;; Advent of code 2020: day 14
;;; see https://adventofcode.com/2020/day/14

(defun parse-input ()
  (mapcar #'parse-line (file-lines (my-input-path))))

(defun parse-line (line)
  (cond
    ((starts-with-subseq "mask = " line)
     (cons :mask (subseq line (length "mask = "))))
    (t
     (destructuring-bind (left right) (split-sequence #\= line)
       (let ((index (parse-integer left :start (1+ (position #\[ left))
					:end (position #\] left)))
	     (value (parse-integer right :start 1)))
	 (cons index value))))))

(defun mask-and (mask-string)
  (parse-integer (map 'string (lambda (char) (case char (#\X #\1)
						   (otherwise #\0)))
		      mask-string)
		 :radix 2))

(defun mask-or (mask-string)
  (parse-integer (map 'string (lambda (char) (case char (#\1 #\1)
						   (otherwise #\0)))
		      mask-string)
		 :radix 2))


(defun part-1 (&optional (data (parse-input)))
  (let ((memory (make-hash-table)))
    (loop
      with logand = -1 and logior = 0
      for (flag-or-address . value) in data
      when (eq :mask flag-or-address)
	do (setf logand (mask-and value)
		 logior (mask-or value))
      else
	do (setf (gethash flag-or-address memory)
		 (logior logior (logand logand value))))
    (loop for value being the hash-value of memory
	  sum value)))

(defun map-bit-subset (function n &optional (logior 0))
  (check-type n (integer 0))
  (cond
    ((= n 0)
     (funcall function logior))
    (t
     (let* ((mask (ash 1 (1- (integer-length n))))
	    (next (logxor n mask)))
       (map-bit-subset function next logior)
       (map-bit-subset function next (logior logior mask))
       (values)))))

(defun part-2 (&optional (data (parse-input)))
  (let ((memory (make-hash-table)))
    (loop
      with logand = 0 and logior = 0
      for (flag-or-address . value) in data
      when (eq :mask flag-or-address)
	do (setf logand (mask-and value) ;; 1 for X's
		 logior (mask-or value)) ;; 1 for 1's
      else
	do (map-bit-subset
	    (lambda (subset)
	      (setf (gethash (logior subset logior
				     (logand (lognot logand)
					     flag-or-address))
			     memory)
		    value))
	    logand))
    (loop for value being the hash-value of memory
	  sum value)))
