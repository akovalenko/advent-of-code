(in-package #:advent-of-code.2022.10)

;;; Advent of code 2022: day 10
;;; see https://adventofcode.com/2022/day/10

(defun parse-line (line)
  (let ((*package* (find-package :keyword)))
    (with-input-from-string (in (concatenate 'string "(" line ")"))
      (read in))))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)))
  (let ((cycle 1)
	(sum 0)
	(x 1))
    (flet ((next ()
	     (when (zerop (mod (- cycle 20) 40))
	       (incf sum (* cycle x)))
	     (incf cycle)))
      (loop for (insn arg) in data
	    do (ecase insn
		 (:noop
		  (next))
		 (:addx
		  (next)
		  (next)
		  (incf x arg))))
      sum)))

(defun part-2 (&optional (data (parse-input)))
  (let ((cycle 1)
	(screen (make-array '(6 40) :initial-element nil))
	(x 1))
    (flet ((next ()
	     (multiple-value-bind (sy sx)
		 (floor (1- cycle) 40)
	       (when (<= (abs (- x sx)) 1)
		 (setf (aref screen sy sx) t)))
	     (incf cycle)))
      (loop for (insn arg) in data
	    do (ecase insn
		 (:noop
		  (next))
		 (:addx
		  (next)
		  (next)
		  (incf x arg))))
      (loop for sy below 6
	    do (loop for sx below 40
		     do (write-char (if (aref screen sy sx) #\# #\.)))
	       (write-line "")))))

