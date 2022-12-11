(in-package #:advent-of-code.2022.11)

;;; Advent of code 2022: day 11
;;; see https://adventofcode.com/2022/day/11

(defstruct (monkey (:type vector))
  id items expr divby if-true if-false inspected)

(defun parse-input ()
  (let ((*package* (find-package :keyword)))
    (coerce
     (loop for group in (line-groups (file-lines (my-input-path) :remove-empty nil))
	   collect (make-monkey
		    :id (parse-integer (subseq (pop group) 7) :junk-allowed t)
		    :items (let* ((line (pop group))
				  (colon (position #\: line)))
			     (mapcar 'parse-integer
				     (split-sequence #\, (subseq line (1+ colon)))))
		    :expr (let* ((line (pop group))
				 (colon (position #\: line)))
			    (read-from-string
			     (concatenate 'string "(" (subseq line (1+ colon)) ")")))
		    :divby (let* ((line (pop group))
				  (colon (position #\: line)))
			     (car (last (read-from-string
					 (concatenate 'string "(" (subseq line (1+ colon)) ")")))))
		    :if-true (let* ((line (pop group))
				    (colon (position #\: line)))
			       (car (last (read-from-string
					   (concatenate 'string "(" (subseq line (1+ colon)) ")")))))
		    :if-false (let* ((line (pop group))
				     (colon (position #\: line)))
				(car (last (read-from-string
					    (concatenate 'string "(" (subseq line (1+ colon)) ")")))))
		    :inspected 0))
     'vector)))

(defun mapply (expr val)
  (destructuring-bind (new eq old op b) expr
    (declare (ignorable new eq old))
    (ecase op
      (:+ (+ val b))
      (:* (if (eq :old b)
	      (* val val)
	      (* val b))))))

(defun part-1 (&optional (data (parse-input)))
  (dotimes (round 20)
    (loop for monkey across data
	  do (loop for item in (shiftf (monkey-items monkey) nil)
		   do (incf (monkey-inspected monkey))
		      (let ((new (mapply (monkey-expr monkey) item)))
			(let ((new (floor new 3)))
			  (let ((next (if (zerop (mod new (monkey-divby monkey)))
					  (monkey-if-true monkey)
					  (monkey-if-false monkey))))
			    (appendf (monkey-items (aref data next))
				     (list new))))))))
  (apply '* (subseq (sort (map 'list 'monkey-inspected data) #'>) 0 2)))

(defun part-2 (&optional (data (parse-input)))
  (let ((lcm (apply 'lcm (map 'list 'monkey-divby data))))
    (dotimes (round 10000)
      (loop for monkey across data
	    do (loop for item in (shiftf (monkey-items monkey) nil)
		     do (incf (monkey-inspected monkey))
			(let ((new (mod (mapply (monkey-expr monkey) item) lcm)))
			  (let ((next (if (zerop (mod new (monkey-divby monkey)))
					  (monkey-if-true monkey)
					  (monkey-if-false monkey))))
			    (appendf (monkey-items (aref data next))
				     (list new))))))))
  (apply '* (subseq (sort (map 'list 'monkey-inspected data) #'>) 0 2)))
