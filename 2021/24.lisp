(in-package #:advent-of-code.2021.24)

;;; Advent of code 2021: day 24
;;; see https://adventofcode.com/2021/day/24

(defun parse-line (string)
  (let ((*package* (find-package :keyword)))
    (read-from-string (format nil "(~a)" string))))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun regno (symbol)
  (case symbol (:w 0) (:x 1) (:y 2) (:z 3)))

(defun %eql (x y)
  (if (eql x y) 1 0))

(defun run (prog input)
  (loop with reg = (vector 0 0 0 0)
	for (insn a b) in prog
	do (setf (aref reg (regno a))
		  (case insn
		    (:inp (pop input))
		    (otherwise
		     (let ((b (if (regno b) (aref reg (regno b)) b)))
		       (funcall (ecase insn
				  (:add '+)
				  (:mul '*)
				  (:div 'truncate)
				  (:eql '%eql)
				  (:mod 'mod))
				(aref reg (regno a)) b)))))
	finally (return (aref reg (regno :z)))))

(defparameter *chunk-template*
  '((:inp :w)
    (:mul :x 0)
    (:add :x :z)
    (:mod :x 26)
    (:div :z (26 1))
    (:add :x *)
    (:eql :x :w)
    (:eql :x 0)
    (:mul :y 0)
    (:add :y 25)
    (:mul :y :x)
    (:add :y 1)
    (:mul :z :y)
    (:mul :y 0)
    (:add :y :w)
    (:add :y *)
    (:mul :y :x)
    (:add :z :y)))

(defun check-program (prog)
  (loop with pattern = nil
	and parameters = nil
	for item in prog
	for (op a b) = item
	for item2 = (and (or pattern (setf pattern *chunk-template*))
			 (pop pattern))
	for (op1 a1 b1) = item2
	do (assert (eq op op1))
	   (assert (eql a a1))
	   (unless (eql b b1)
	     (etypecase b1
	       (null)
	       (list
		(assert (member b b1))
		(push b parameters))
	       ((member *)
		(push b parameters))))
	finally (return (loop with p = (nreverse parameters)
			      while p
			      collect (list (pop p) (pop p) (pop p))))))

;;; at least with my input, each chunk starting with (:INP :W) uses
;;; three parameters (divisor = 1 or 26, Dx, Dy) and the previous
;;; value of Z. First, guess success is evaluated as
;;;
;;;   (EQL :W (+ Dx (Mod Z 26)))
;;; 
;;; for divisor = 1, on success Z doesn't change, while on failure
;;; Z becomes (+ Dy :W (* Z 26)).
;;; 
;;; for divisor = 26, on success Z is set to (truncate Z 26), while on
;;; failure, to (+ Dy :W (* 26 (truncate Z 26))).
;;;
;;; The trick is, Dx values make it impossible to guess with divisor =
;;; 1, and Dy values are such that (+ Dy :W) is always a valid
;;; NON-ZERO base-26 digit.
;;;
;;; Hence divisor=1 ("push") ops extend Z by one base26 digit at the
;;; end, and divisor=26 ("pop") ops reduce Z by one base26 digit if we
;;; guess :W correctly.
;;;
;;; Another goodie is that there are exactly 7 pushes and 7 pops
;;; total, so for each pop, we HAVE to guess correctly in order to get
;;; zero as the final value of Z.

(defun check-invariants (param)
  (loop for (div dx dy) in param
	do (assert (typep (+ dy 9) '(mod 26)))
	   (assert (typep (+ dy 1) '(mod 26)))
	   (assert (not (<= 1 (mod (- dy) 26) 9)))
	   (assert (or (= div 26) (< 9 dx))))
  (assert
   (= 7
      (count 26 param :key 'first)
      (count 1 param :key 'first)))
  param)

;;; every "pop" chunk, when it's known to have a successfull guess,
;;; establishes a relation between current digit and some previous
;;; digit: current = current-dx + old-y + old-digit.  Let's collect
;;; such relations as three-element lists: (current old delta), where
;;; current-input = old-input + delta
(defun digit-relations (param)
  (loop
    with digit-stack = nil
    and relations = nil
    for (div dx dy) in param
    for input from 0
    when (= 1 div)
      do (push (list dy input) digit-stack)
    else
      do (destructuring-bind (old-dy old-input) (pop digit-stack)
	   ;; we have inp#input = dx + old-dy + inp#old-input
	   (push (list input old-input (+ dx old-dy)) relations))
    finally (return relations)))

;;; 7 pushes and 7 pops give us 7 unique inter-digit relations, where
;;; two digits are paired and their difference is established, so it's
;;; clear which one is greater. Here we set the greater one to 9 and
;;; derive the other.

(defun maximum-inputs (relations)
  (loop with inputs = (make-sequence 'vector 14 :initial-element nil)
	for (this other delta) in relations
	when (plusp delta)
	  do (setf (aref inputs this) 9
		   (aref inputs other) (- 9 delta))
	else
	  do (setf (aref inputs other) 9
		   (aref inputs this) (+ 9 delta))
	finally (assert (notany 'null inputs))
		(return (format nil "~{~a~}" (coerce inputs 'list)))))

(defun part-1 (&optional (input (parse-input)))
  (maximum-inputs
   (digit-relations
    (check-invariants
     (check-program input)))))

;;; 7 pushes and 7 pops give us 7 unique inter-digit relations, where
;;; two digits are paired and their difference is established, so it's
;;; clear which one is smaller. Here we set the smaller one to 1 and
;;; derive the other.

(defun minimum-inputs (relations)
  (loop with inputs = (make-sequence 'vector 14 :initial-element nil)
	for (this other delta) in relations
	when (plusp delta)
	  do (setf (aref inputs this) (1+ delta)
		   (aref inputs other) 1)
	else
	  do (setf (aref inputs this) 1
		   (aref inputs other) (- 1 delta))
	finally (assert (notany 'null inputs))
		(return (format nil "~{~a~}" (coerce inputs 'list)))))

(defun part-2 (&optional (input (parse-input)))
  (minimum-inputs
   (digit-relations
    (check-invariants
     (check-program input)))))
