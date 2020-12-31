(in-package #:advent-of-code.2020.10)

;;; Advent of code 2020: day 10
;;; see https://adventofcode.com/2020/day/10

(defun parse-input ()
  (mapcar 'parse-integer (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)))
  (let ((seq (let ((jolts (sort (concatenate 'vector data) #'<)))
	       (loop for i below (1- (length jolts))
		     collect (- (aref jolts (1+ i))
				(aref jolts i))))))
    (* (1+ (count 3 seq)) (1+ (count 1 seq)))))

(defun part-2 (&optional (data (parse-input)))
  (let* ((seq (concatenate 'vector data))
	 (length (+ 2 (length seq)))
	 (max (reduce #'max seq))
	 (seq (sort (concatenate 'vector '(0) seq (list (+ 3 max))) #'<))
	 (cache (make-array (list length length) :initial-element nil)))
    (labels ((ways (i j)
	       (or (aref cache i j)
		   (setf (aref cache i j)
			 (+ (if (<= (- (aref seq j) (aref seq i)) 3) 1 0)
			    (loop for k from (1+ i) below j
				  while (<= (- (aref seq k) (aref seq i)) 3)
				  sum (ways k j)))))))
      (ways 0 (1- (length seq))))))
