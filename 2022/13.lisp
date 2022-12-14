(in-package #:advent-of-code.2022.13)

;;; Advent of code 2022: day 13
;;; see https://adventofcode.com/2022/day/13

(defun parse-input ()
  (loop for group in (line-groups (file-lines (my-input-path) :remove-empty nil))
	collect (mapcar
		 (lambda (line)
		   (read-from-string
		    (substitute #\Space #\, (substitute #\) #\] (substitute #\( #\[ line)))))
		 group)))

(defun compare (a b)
  (cond
    ((and (integerp a)
	  (integerp b))
     (if (< a b)
	 '<
	 (if (> a b)
	     '> '=)))
    ((and (listp a)
	  (listp b))
     (if (and (endp a)
	      (endp b))
	 '=
	 (if (endp a)
	     '<
	     (if (endp b)
		 '>
		 (case (compare (first a) (first b))
		   (= (compare (rest a) (rest b)))
		   (< '<)
		   (> '>))))))
    (t
     (compare (if (listp a) a (list a))
	      (if (listp b) b (list b))))))

(defun part-1 (&optional (data (parse-input)))
  (loop for index from 1
	for (left right) in data
	for compare = (compare left right)
	when (eq '< compare)
	  sum index))

(defun part-2 (&optional (data (parse-input)))
  (let ((data (loop for (left right) in data
		    nconc (list left right))))
    (push '((2)) data)
    (push '((6)) data)
    (setf data (sort data (lambda (left right)
			    (eq '< (compare left right)))))
    (* (1+ (position '((2)) data :test 'equal))
       (1+ (position '((6)) data :test 'equal)))))
