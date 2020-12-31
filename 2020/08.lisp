(in-package #:advent-of-code.2020.08)

;;; Advent of code 2020: day 08
;;; see https://adventofcode.com/2020/day/8

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun parse-line (line)
  (cons (intern (string-upcase (subseq line 0 3)) :keyword)
	(parse-integer line :start 4)))

(defun run-code (code)
  (let ((accumulator 0)
	(ip 0))
    (let ((visited (make-sequence 'bit-vector (length code) :initial-element 0)))
      (loop
	(when (= ip (length code))
	  (return (values accumulator :terminated)))
	(when (= 1 (shiftf (bit visited ip) 1))
	  (return (values accumulator :loop)))

	(destructuring-bind (insn . arg) (aref code ip)
	  (ecase insn
	    (:jmp (incf ip (1- arg)))
	    (:nop)
	    (:acc (incf accumulator arg)))
	  (incf ip))))))

(defun part-1 (&optional (data (parse-input)))
  (multiple-value-bind (accumulator state)
      (run-code (concatenate 'vector data))
    (assert (eq :loop state))
    accumulator))

(defun part-2 (&optional (data (parse-input)))
  (let* ((code (concatenate 'vector data)))
    (flet ((toggle (index)
	     (setf (car (aref code index))
		   (getf '(:jmp :nop :nop :jmp)
			 (car (aref code index))))))
     (loop for index from 0 below (length code)
	   when (member (car (aref code index)) '(:jmp :nop))
	     do (toggle index)
		(multiple-value-bind (accumulator state)
		    (run-code code)
		  (toggle index)
		  (when (eq state :terminated)
		    (return accumulator)))))))
