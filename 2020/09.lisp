(in-package #:advent-of-code.2020.09)

;;; Advent of code 2020: day 09
;;; see https://adventofcode.com/2020/day/9

(defun parse-input ()
  (mapcar 'parse-integer (file-lines (my-input-path))))


(defun part-1 (&optional (data (parse-input)) (chunk 25))
  (let* ((numbers (concatenate 'vector data))
	 (registry (make-hash-table))
	 (length (length numbers)))
    (flet ((notice (a b)
	     (and (/= a b)
		  (incf (gethash (+ a b) registry 0))))
	   (unnotice (a b)
	     (and (/= a b)
		  (when (zerop (decf (gethash (+ a b) registry)))
		    (remhash (+ a b) registry)))))
      (loop for i below (1- chunk)
	    do (loop for j from (1+ i) below chunk
		     do (notice (aref numbers i)
				(aref numbers j))))
      (loop with head = 0
	    for i from chunk below length
	    for item = (aref numbers i)
	    do (unless (gethash item registry)
		 (return item))
					; lose old
	       (loop for j from head below (+ head chunk)
		     do (unnotice (aref numbers j)
				  (aref numbers head)))
	       (incf head)
	       (loop for j from head below (+ head chunk)
		     do (notice (aref numbers j)
				item))))))


(defun part-2 (&optional (data (parse-input)))
  (let ((sum (part-1 data 25))
	(numbers (concatenate 'vector data)))
    (let ((length (length numbers)))
      (loop for chunk from 2 to length
	    do (loop with acc = (loop for i below chunk
				      sum (aref numbers i))
		     for head from 0 below (- length chunk)
		     do (when (= acc sum)
			  (return-from part-2
			    (+ (reduce 'min numbers :start head :end (+ head chunk))
			       (reduce 'max numbers :start head :end (+ head chunk)))))
			(decf acc (aref numbers head))
			(incf acc (aref numbers (+ head chunk))))))))
