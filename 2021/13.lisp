(in-package #:advent-of-code.2021.13)

;;; Advent of code 2021: day 13
;;; see https://adventofcode.com/2021/day/13

(defun parse-input ()
  (destructuring-bind (dots folds)
      (line-groups (file-lines (my-input-path) :remove-empty nil))
    (flet ((parse-dot (string)
	     (apply 'complex (mapcar 'parse-integer
				     (split-sequence #\, string))))
	   (parse-fold (string)
	     (assert (starts-with-subseq "fold along " string))
	     (cons (char= #\x (char string #.(length "fold along ")))
		   (parse-integer string :start #.(length "fold along _=")))))
      (list (mapcar #'parse-dot dots)
	    (mapcar #'parse-fold folds)))))

(defun fold-value (coord line)
  (cond ((< coord line) coord)
	((< line coord) (- (* 2 line) coord))
	(t (error "dot on fold"))))

(defun fold-image (dots-hash by-x line)
  (let ((new-dots (make-hash-table)))
    (flet ((fold-dot (dot)
	     (if by-x
		 (complex
		  (fold-value (realpart dot) line)
		  (imagpart dot))
		 (complex
		  (realpart dot)
		  (fold-value (imagpart dot) line)))))
      (maphash (lambda (key value)
		 (declare (ignore value))
		 (setf (gethash (fold-dot key) new-dots) t))
	       dots-hash))
    new-dots))

(defun part-1 (&optional (data (parse-input)))
  (destructuring-bind (dots folds) data
    (destructuring-bind (by-x . line) (first folds)
      (hash-table-count
       (fold-image
	(alist-hash-table
	 (loop for dot in dots
	       collect (cons dot t)))
	by-x line)))))

(defun part-2 (&optional (data (parse-input)))
  (destructuring-bind (dots folds) data
    (let ((image
	    (alist-hash-table
	     (loop for dot in dots
		   collect (cons dot t)))))
      (loop for (by-x . line) in folds
	    do (setf image (fold-image image by-x line)))
      (let ((dots (hash-table-keys image)))
	(loop for y to (reduce 'max dots :key 'imagpart)
	      do (loop for x to (reduce 'max dots :key 'realpart)
		       do (write-char (if (gethash (complex x y) image) #\# #\.)))
	     (write-line ""))))))

