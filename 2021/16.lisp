(in-package #:advent-of-code.2021.16)

;;; Advent of code 2021: day 16
;;; see https://adventofcode.com/2021/day/16

(defun parse-input ()
  (first (file-lines (my-input-path))))

(defun bit-vectorize (hex-string)
  (let ((bits (make-sequence 'simple-bit-vector (* 4 (length hex-string)))))
    (dotimes (i (length hex-string) bits)
      (let ((digit (digit-char-p (char hex-string i) 16)))
	(dotimes (j 4)
	  (setf (sbit bits (+ (* i 4) j))
		(ldb (byte 1 (- 3 j)) digit)))))))

(defun parse-packet-stream (bits)
  (let ((cursor 0)
	(vsum 0))
    (labels ((msb (n)
	       (unless (<= (+ n cursor) (length bits))
		 (return-from parse-packet-stream vsum))
	       (let ((value 0))
		 (dotimes (i n value)
		   (setf value (logior (ash value 1)
				       (sbit bits cursor)))
		   (incf cursor))))
	     (nsubs (n)
	       (dotimes (i n)
		 (eat1p)))
	     (nbits (n)
	       (print (list 'nbits n cursor))
	       (let ((limit (+ n cursor)))
		 (loop while (< cursor limit)
		       do (eat1p))))
	     (eat1p ()
	       (let ((ver (msb 3))
		     (type-id (msb 3)))
		 (incf vsum ver)
		 (print (list cursor ver type-id (length bits)))
		 (case type-id
		   (4
		    (loop while (logbitp 4 (print (msb 5)))))
		   (otherwise
		    (case (msb 1)
		      (0 (nbits (msb 15)))
		      (1 (nsubs (msb 11)))))))))
      (loop while (< 5 (- (length bits) cursor))
	    do (eat1p))
      vsum)))


(defun eval-packet-stream (bits)
  (let ((cursor 0))
    (labels ((fn (type-id values)
	       (ecase type-id
		 (0 (reduce '+ values))
		 (1 (reduce '* values))
		 (2 (reduce 'min values))
		 (3 (reduce 'max values))
		 (5 (if (apply '> values) 1 0))
		 (6 (if (apply '< values) 1 0))
		 (7 (if (apply '= values) 1 0))))
	     (msb (n)
	       (let ((value 0))
		 (dotimes (i n value)
		   (setf value (logior (ash value 1)
				       (sbit bits cursor)))
		   (incf cursor))))
	     (nsubs (n)
	       (loop repeat n collect (eat1p)))
	     (nbits (n)
	       (let ((limit (+ n cursor)))
		 (loop while (< cursor limit)
		       collect (eat1p))))
	     (eat1p ()
	       (let ((ver (msb 3))
		     (type-id (msb 3)))
		 (case type-id
		   (4
		    (loop with value = 0
			  for digit = (msb 5)
			  do (setf value (logior (ash value 4) (ldb (byte 4 0) digit)))
			  while (logbitp 4 digit)
			  finally (return value)))
		   (otherwise
		    (case (msb 1)
		      (0 (fn type-id (nbits (msb 15))))
		      (1 (fn type-id (nsubs (msb 11))))))))))
      (eat1p))))

(defun part-1 (&optional (data (parse-input)))
  (parse-packet-stream (bit-vectorize data)))

(defun part-2 (&optional (data (parse-input)))
  (eval-packet-stream (bit-vectorize data)))

