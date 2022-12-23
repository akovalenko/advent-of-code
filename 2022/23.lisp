(in-package #:advent-of-code.2022.23)

;;; Advent of code 2022: day 23
;;; see https://adventofcode.com/2022/day/23

(defun parse-input ()
  (file-lines (my-input-path)))

(defun input-elve-hash (input)
  (let ((hash-table (make-hash-table)))
    (loop for line in input
	  for y from 0
	  do (loop for char across line
		   for x from 0
		   when (char= char #\#)
		     do (setf (gethash (complex x y) hash-table) t)))
    hash-table))

(defparameter +nswe+ #(#c(0 -1) #c(0 1) #c(-1 0) #c(1 0)))

(defparameter +ortho-deltas+ #(#c(1 0) #c(1 0) #c(0 1) #c(0 1)))

(defun ground-in-containing-rectangle (hash-table)
  (let ((points (hash-table-keys hash-table)))
   (let ((min-x (reduce 'min points :key 'realpart))
	 (max-x (reduce 'max points :key 'realpart))
	 (min-y (reduce 'min points :key 'imagpart))
	 (max-y (reduce 'max points :key 'imagpart)))
     (- (* (1+ (- max-x min-x))
	   (1+ (- max-y min-y)))
	(hash-table-count hash-table)))))

(defun print-field (hash-table &optional ul)
  (let ((points (hash-table-keys hash-table)))
    (let ((min-x (if ul (realpart ul) (1- (reduce 'min points :key 'realpart))))
	  (max-x (1+ (reduce 'max points :key 'realpart)))
	  (min-y (if ul (imagpart ul) (1- (reduce 'min points :key 'imagpart))))
	  (max-y (1+ (reduce 'max points :key 'imagpart))))
      (loop for y from min-y to max-y
	    do (loop for x from min-x to max-x
		     do (write-char (if (gethash (complex x y) hash-table) #\# #\.)))
	       (write-line "")))))

(defun propose-move (elf field phase)
  (and (loop for dx from -1 to 1 ;; has neighbours
	       thereis
	       (loop for dy from -1 to 1
		       thereis
		       (and (not (= dx dy 0))
			    (gethash (+ elf (complex dx dy)) field))))
       (dotimes (step 4)
	 (let* ((dir (mod (+ step phase) 4))
		(delta (aref +nswe+ dir))
		(ortho (aref +ortho-deltas+ dir)))
	   (when (loop for probe in (list ortho 0 (- ortho))
		       never (gethash (+ elf delta probe) field))
	     (return (+ elf delta)))))))

(defun make-move (field phase)
  (let ((targets (make-hash-table)))
    (loop for elf in (hash-table-keys field)
	  for destination = (propose-move elf field phase)
	  when destination
	    do (incf (gethash destination targets 0)))
    (when (= 0 (hash-table-count targets))
      (return-from make-move (values field t)))
    (loop
      with new-field = (make-hash-table)
      for elf in (hash-table-keys field)
      for destination = (propose-move elf field phase)
      for new-elf = elf
      when (= 1 (gethash destination targets 0))
	do (setf new-elf destination)
      do (setf (gethash new-elf new-field) t)
      finally (return new-field))))

(defun part-1 (&optional (data (parse-input)))
  (let ((field (input-elve-hash data)))
    (loop for phase below 10
	  do (setf field (make-move field phase)))
    (ground-in-containing-rectangle field)))

(defun part-2 (&optional (data (parse-input)))
  (let ((field (input-elve-hash data)))
    (loop for phase from 0
	  do (multiple-value-bind (new-field stationary)
		 (make-move field phase)
	       (setf field new-field)
	       (when stationary
		 (return (1+ phase)))))))
