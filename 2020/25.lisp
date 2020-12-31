(in-package #:advent-of-code.2020.25)

;;; Advent of code 2020: day 25
;;; see https://adventofcode.com/2020/day/25

(defun parse-input ()
  (mapcar 'parse-integer (file-lines (my-input-path))))

(defparameter *modulus* 20201227)

(defun discrete-log (n)
  (loop for value = 1 then (mod (* value 7) *modulus*)
	for step from 0
	when (= value n)
	  return step))

(defun mod-expt (base power &optional (modulus *modulus*))
  (let ((value 1))
   (loop
     (cond ((= 0 power)
	    (return value))
	   ((evenp power)
	    (setf base (mod (* base base) modulus)
		  power (ash power -1)))
	   (t
	    (setf value (mod (* value base) modulus)
		  power (1- power)))))))

(defun private-keys (&optional (data (parse-input)))
  (loop for pubkey in data
	collect (discrete-log pubkey) into private-keys
	finally (return
		  private-keys)))

(defun part-1 (&optional (data (parse-input)))
  (destructuring-bind (pub1 pub2) data
    (mod-expt pub1 (discrete-log pub2))))
