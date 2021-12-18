(in-package #:advent-of-code.2021.18)

;;; Advent of code 2021: day 18
;;; see https://adventofcode.com/2021/day/18


(defstruct (ptr (:type vector))
  (setter #'identity)
  (getter #'values))

(defun at (ptr) (funcall (ptr-getter ptr)))
(defun (setf at) (v ptr) (funcall (ptr-setter ptr) v))

(defmacro to (place)
  (let ((v (gensym "V")))
    `(make-ptr :setter (lambda (,v) (setf ,place ,v))
	       :getter (lambda () ,place))))

(defparameter +zerohole+
  (make-ptr :getter (constantly 0)))

(defun consify (lol)
  (if (listp lol)
      (cons (consify (first lol)) (consify (second lol)))
      lol))

(defun parse-cons-tree (line)
  (consify
   (read-from-string
    (map 'string (lambda (char)
		   (case char
		     (#\, #\Space)
		     (#\[ #\()
		     (#\] #\))
		     (otherwise char)))
	 line))))

(defun parse-input ()
  (mapcar 'parse-cons-tree (file-lines (my-input-path))))

(defun explodable (ptr)
  (let ((left +zerohole+)
	(pair nil))
    (labels
	((visit (ptr depth)
	   (let ((value (at ptr)))
	     (if (consp value)
		 (if (and (not pair)
			  (= depth 4))
		     (setf pair ptr)
		     (progn
		       (visit (to (car value)) (1+ depth))
		       (visit (to (cdr value)) (1+ depth))))
		 (if pair
		     (return-from explodable
		       (values pair left ptr))
		     (setf left ptr))))))
      (visit ptr 0)
      (when pair
	(values pair left +zerohole+)))))

(defun maybe-explode (ptr)
  (multiple-value-bind (pair left right)
      (explodable ptr)
    (when pair
      (destructuring-bind (a . b) (at pair)
	(incf (at left) a)
	(incf (at right) b)
	(setf (at pair) 0)
	t))))

(defun splittable (ptr)
  (labels
      ((visit (ptr)
	 (let ((value (at ptr)))
	   (if (consp value)
	       (progn
		 (visit (to (car value)))
		 (visit (to (cdr value))))
	       (when (<= 10 value)
		 (return-from splittable ptr))))))
    (visit ptr)
    nil))

(defun maybe-split (ptr)
  (let ((atom (splittable ptr)))
    (when atom
      (setf (at atom)
	    (cons (floor (at atom) 2)
		  (ceiling (at atom) 2)))
      t)))

(defun reduce-snail-number (tree)
  (loop with ptr = (to tree)
	while (or (maybe-explode ptr)
		  (maybe-split ptr)))
  tree)

(defun snn-add (a b)
  (reduce-snail-number (cons a b)))

(defun mag (s)
  (if (consp s)
      (+ (* 3 (mag (car s)))
	 (* 2 (mag (cdr s))))
      s))

(defun part-1 (&optional (data (parse-input)))
  (mag (reduce 'snn-add data)))

(defun part-2 (&optional (data (parse-input)))
  (loop for a in data
	maximize
	(loop for b in data
	      unless (eq a b)
		maximize
		(mag (snn-add (copy-tree a) (copy-tree b))))))
