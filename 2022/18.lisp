(in-package #:advent-of-code.2022.18)

;;; Advent of code 2022: day 18
;;; see https://adventofcode.com/2022/day/18

(defun parse-line (line)
  (mapcar 'parse-integer (split-sequence #\, line)))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun cube-neighbors (xyz)
  (loop for sign in '(1 -1)
	nconc (loop for i below 3
		    for copy = (copy-list xyz)
		    do (incf (elt copy i) sign)
		    collect copy)))

(defun part-1 (&optional (data (parse-input)))
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (cube data)
      (setf (gethash cube hash) t))
    (loop for cube in data
	  sum (loop for other in (cube-neighbors cube)
		    count (null (gethash other hash))))))

(defun part-2 (&optional (data (parse-input)))
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (cube data)
      (setf (gethash cube hash) :cube))
    (let ((limits (loop for i below 3
			collect (1+ (loop for cube in data
					  maximize (elt cube i))))))
      (flet ((in-area (cube)
	       (every 'identity (mapcar '<= '(-1 -1 -1) cube limits))))
	(let ((front (list limits))
	      (next (make-hash-table :test 'equal)))
	  (loop while front
		do (dolist (point front)
		     (setf (gethash point hash) :air)
		     (dolist (around (cube-neighbors point))
		       (when (in-area around)
			 (unless (gethash around hash)
			   (setf (gethash around next) t)))))
		   (setf front (hash-table-keys next))
		   (clrhash next))
	  (loop for cube in data
		sum (loop for other in (cube-neighbors cube)
			  count (eq :air (gethash other hash)))))))))
