(in-package #:advent-of-code.2020.13)

;;; Advent of code 2020: day 13
;;; see https://adventofcode.com/2020/day/13

(defun parse-input ()
  (destructuring-bind (start-time buses)
      (file-lines (my-input-path))
    (list (parse-integer start-time)
	  (loop for id in (split-sequence #\, buses)
		collect (if (string= id "x")
			    nil
			    (parse-integer id))))))

(defun part-1 (&optional (data (parse-input)))
  (destructuring-bind (start-time buses) data
    (setf buses (remove nil buses))
    (let ((to-wait)
	  (best-bus))
      (dolist (id buses (* to-wait best-bus))
	(let ((arrives-in (- id (mod start-time id))))
	  (when (or (not to-wait)
		    (< arrives-in to-wait))
	    (setf to-wait arrives-in
		  best-bus id)))))))

;;; borrowed from cl:ironclad
(defun modular-inverse (n modulus)
  "Returns M such that N * M mod MODULUS = 1"
  (declare (type (integer 1 *) modulus))
  (declare (type (integer 0 *) n))
  (when (or (zerop n) (and (evenp n) (evenp modulus)))
    (return-from modular-inverse 0))
  (loop
    with r1 of-type integer = n
    and r2 of-type integer = modulus
    and u1 of-type integer = 1
    and u2 of-type integer = 0
    and q of-type integer = 0
    and r of-type integer = 0
    until (zerop r2)
    do (progn
         (multiple-value-setq (q r) (floor r1 r2))
         (setf r1 r2
               r2 r)
         (decf u1 (* q u2))
         (rotatef u1 u2))
    finally (return (let ((inverse u1))
                      (when (minusp inverse)
                        (setf inverse (mod inverse modulus)))
                      (if (zerop (mod (* n inverse) modulus))
                          0
                          inverse)))))

(defun solve-china-remainders (remainder-modulus-alist)
  (dolist (i remainder-modulus-alist)
    (dolist (j remainder-modulus-alist)
      (assert (or (eq i j) (= 1 (gcd (cdr i) (cdr j)))))))
  (let ((m (reduce '* (mapcar 'cdr remainder-modulus-alist))))
    (mod (loop for (rem . mod) in remainder-modulus-alist
	       for m-i = (floor m mod)
	       for m-i-inverted = (modular-inverse m-i mod)
	       sum (* rem m-i m-i-inverted))
	 (reduce 'lcm (mapcar 'cdr remainder-modulus-alist)))))

(defun part-2 (&optional (data (parse-input)))
  (let ((bus-ids-and-gaps (second data)))
    (solve-china-remainders
     (loop for wait-for from 0
	   for id in bus-ids-and-gaps
	   when id
	     collect (cons (mod (- wait-for) id) id)))))
