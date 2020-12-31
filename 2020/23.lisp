(in-package #:advent-of-code.2020.23)

;;; Advent of code 2020: day 23
;;; see https://adventofcode.com/2020/day/23

(defun parse-input ()
  (first (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)))
  (let ((crabs (apply 'circular-list (map 'list 'digit-char-p data))))
    (flet ((forth ()
	     (let ((stash))
	       (push (pop (cdr crabs)) stash)
	       (push (pop (cdr crabs)) stash)
	       (push (pop (cdr crabs)) stash)
	       (let ((next (loop with i = (car crabs)
				 do (when (= i (car crabs)) (decf i))
				    (when (= 0 i) (setf i 9))
				    (unless (find i stash)
				      (return (member i crabs)))
				    (decf i))))

		 (push (pop stash) (cdr next))
		 (push (pop stash) (cdr next))
		 (push (pop stash) (cdr next))))
	     (pop crabs))
	   (state ()
	     (loop for crab in (member 1 crabs)
		   repeat 9
		   collect crab)))
      (dotimes (i 100 (subseq (map 'string 'digit-char (state)) 1))
	(forth)))))

(defun part-2 (&optional (data (parse-input)) (steps 10000000) (upto 1000000))
  (let ((crabs data))
    (declare (optimize (speed 3))
	     (string crabs)
	     (fixnum steps upto))

    (let* ((length (length crabs))
	   (ram (make-array (list (1+ upto)) :element-type '(unsigned-byte 32)))
	   (list (map 'list 'digit-char-p crabs))
	   (pick-up (make-array 3 :element-type '(unsigned-byte 32))))

      (loop for (head next) on list
	    while next
	    do (setf (aref ram head) next)
	    finally (if (= length upto)
			(setf (aref ram head) (car list))
			(setf (aref ram head) (1+ length))))

      (loop for i from (1+ length) to (1- upto)
	    do (setf (aref ram i) (1+ i)))

      (unless (= length upto)
	(setf (aref ram upto) (car list)))
      (setf (aref ram 0) (car list))

      (labels ((after (n)
		 (aref ram n))

	       (index-1 (n)
		 (1+ (mod (- n 2) upto)))

	       (display ()
		 (loop with i = (after 0)
		       repeat upto
		       collect i
		       do (setf i (after i))))

	       (forth ()
		 (let ((cell (after 0)))
		   (dotimes (i 3)
		     (setf (aref pick-up i)
			   (setf cell (after cell))))
		   ;; where to attarch next
		   (setf cell (after 0))
		   (loop
		     (setf cell (index-1 cell))
		     (unless (find cell pick-up)
		       (return)))

					;(pprint (list pick-up '> cell))

		   (setf (aref ram (after 0))
			 (aref ram (aref pick-up 2)))

		   (setf (aref ram (aref pick-up 2))
			 (aref ram cell))

		   (setf (aref ram cell)
			 (aref pick-up 0))

		   (setf (aref ram 0)
			 (aref ram (aref ram 0))))))

					;(pprint (subseq ram 0 (min 20 upto)))
	(dotimes (i steps (apply '* (list (after 1) (after (after 1)))))
	  (forth))))))
