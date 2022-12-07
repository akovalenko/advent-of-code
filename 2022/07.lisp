(in-package #:advent-of-code.2022.07)

;;; Advent of code 2022: day 07
;;; see https://adventofcode.com/2022/day/7

(defun parse-line (line)
  (split-sequence #\Space line))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)))
  (loop
    with hash-table = (make-hash-table :test 'equal)
    with cwd = nil
    for (c1 c2 c3) in data
    do (if (string= c1 "$")
	   (when (string= c2 "cd")
	     (if (string= c3 "/")
		 (setf cwd nil)
		 (if (string= c3 "..")
		     (setf cwd (rest cwd))
		     (setf cwd (cons c3 cwd)))))
	   (if (string= c1 "dir")
	       (setf (gethash (cons c2 cwd) hash-table) 0)
	       (setf (gethash (cons c2 cwd) hash-table)
		     (parse-integer c1))))
    finally 
       (let ((dir-hash-table (make-hash-table :test 'equal)))
	 (maphash (lambda (k v)
		    (unless (= 0 v)
		      (loop with path = k
			    do (pop path)
			       (incf (gethash path dir-hash-table 0) v)
			       (unless path (return)))))
		  hash-table)
	 (let ((small-dir-sizes 0))
	   (maphash (lambda (k v) k
		      (when (<= v 100000)
			(incf small-dir-sizes v)))
		    dir-hash-table)
	   (return small-dir-sizes)))))

(defun part-2 (&optional (data (parse-input)))
  (loop
    with hash-table = (make-hash-table :test 'equal)
    with cwd = nil
    for (c1 c2 c3) in data
    do (if (string= c1 "$")
	   (when (string= c2 "cd")
	     (if (string= c3 "/")
		 (setf cwd nil)
		 (if (string= c3 "..")
		     (setf cwd (rest cwd))
		     (setf cwd (cons c3 cwd)))))
	   (if (string= c1 "dir")
	       (setf (gethash (cons c2 cwd) hash-table) 0)
	       (setf (gethash (cons c2 cwd) hash-table)
		     (parse-integer c1))))
    finally 
       (return
	 (let ((dir-hash-table (make-hash-table :test 'equal)))
	   (maphash (lambda (k v)
		      (unless (= 0 v)
			(loop with path = k
			      do (pop path)
				 (incf (gethash path dir-hash-table 0) v)
				 (unless path (return)))))
		    hash-table)
	   (let* ((root-size (gethash nil dir-hash-table))
		  (must-free-up (- root-size 40000000))
		  (alist (sort (hash-table-alist dir-hash-table) #'< :key 'cdr)))
	     (loop with dirs = alist
		   while (<  (cdr (first dirs)) must-free-up)
		   do (pop dirs)
		   finally (return (cdr (first dirs)))))))))

