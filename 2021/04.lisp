(in-package #:advent-of-code.2021.04)

;;; Advent of code 2021: day 04
;;; see https://adventofcode.com/2021/day/4

(defun parse-input ()
  (destructuring-bind ((drawn) . grids)
      (line-groups (file-lines (my-input-path) :remove-empty nil))
    (list
     (mapcar 'parse-integer (split-sequence #\, drawn))
     (loop for grid in grids
	   collect (loop for row in grid
			 collect
			 (mapcar 'parse-integer
				 (split-sequence #\Space row
						 :remove-empty-subseqs t)))))))

(defun make-bingo-map (grids)
  (let ((map (make-hash-table)))
    (loop for board from 0
	  for grid in grids
	  do (loop for row in grid
		   for y below 5
		   do (loop for item in row
			    for x below 5
			    do (push (list board x y)
				     (gethash item map)))))
    map))

(defstruct (board-state (:type vector))
  (mark-count
   (make-sequence 'simple-vector 10 :initial-element 0))
  (marked
   (make-array '(5 5) :element-type 'bit :initial-element 0))
  (grid)
  (won nil))

(defun part-1 (&optional (data (parse-input)))
  (destructuring-bind (drawn grids) data
    (let ((map (make-bingo-map grids))
	  (states
	    (map-into (make-array (length grids))
		      (lambda (grid)
			(make-board-state
			 :grid
			 (make-array '(5 5) :initial-contents grid)))
		      grids)))
      (dolist (called drawn)
	(loop for (n x y) in (gethash called map)
	      do (let ((state (aref states n)))
		   (when (= 0 (aref (board-state-marked state) y x))
		     (setf (aref (board-state-marked state) y x) 1)
		     (incf (aref (board-state-mark-count state) x))
		     (incf (aref (board-state-mark-count state) (+ 5 y)))
		     (when (or (= 5 (aref (board-state-mark-count state) x))
			       (= 5 (aref (board-state-mark-count state) (+ 5 y))))
		       (return-from part-1
			 (* (loop for x below 5
				  sum (loop for y below 5
					    when (= 0 (aref (board-state-marked state) y x))
					      sum (aref (board-state-grid state) y x)))
			    called))))))))))


(defun part-2 (&optional (data (parse-input)))
  (let ((last-score))
    (destructuring-bind (drawn grids) data
      (let ((map (make-bingo-map grids))
	    (states
	      (map-into (make-array (length grids))
			(lambda (grid)
			  (make-board-state
			   :grid
			   (make-array '(5 5) :initial-contents grid)))
			grids)))
	(dolist (called drawn)
	  (loop for (n x y) in (gethash called map)
		do (let ((state (aref states n)))
		     (when (and
			    (not (board-state-won state))
			    (= 0 (aref (board-state-marked state) y x)))
		       (setf (aref (board-state-marked state) y x) 1)
		       (incf (aref (board-state-mark-count state) x))
		       (incf (aref (board-state-mark-count state) (+ 5 y)))
		       (when (or (= 5 (aref (board-state-mark-count state) x))
				 (= 5 (aref (board-state-mark-count state) (+ 5 y))))
			 (setf
			  (board-state-won state) t
			  last-score
			  (* (loop for x below 5
				   sum (loop for y below 5
					     when (= 0 (aref (board-state-marked state) y x))
					       sum (aref (board-state-grid state) y x)))
			     called)))))))))
    last-score))
