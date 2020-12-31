(in-package #:advent-of-code.2020.22)

;;; Advent of code 2020: day 22
;;; see https://adventofcode.com/2020/day/22

(defun parse-input ()
  (destructuring-bind (first second)
      (line-groups (file-lines (my-input-path) :remove-empty nil))
    (list (mapcar 'parse-integer (rest first))
	  (mapcar 'parse-integer (rest second)))))

(defun part-1 (&optional (data (parse-input)))
  (labels ((forth (a b)
	     (assert (and (not (endp a)) (not (endp b))))
	     (if (> (car a) (car b))
		 (values-list (nreverse (multiple-value-list (forth b a))))
					; b wins
		 (let ((top-a (pop a))
		       (top-b (pop b)))
		   (if b (progn (push top-b (cdr (last b)))
				(push top-a (cdr (last b)))
				(values a b))
		       (values a (list top-b top-a)))))))

    (loop with a = (first data)
	  with b = (second data)
	  while (and a b)
	  do (multiple-value-setq (a b) (forth a b))

	  finally (return
		    (loop for item in (reverse (or a b))
			  for worth from 1
			  sum (* item worth))))))

(defun part-2 (&optional (data (parse-input)))
  (labels ((forth-normal (a b &optional win)
	     (assert (and (not (endp a)) (not (endp b))))
	     (if (if win (= win 1) (> (car a) (car b)))
		 (values-list (nreverse (multiple-value-list (forth-normal b a (and win (- 3 win))))))
					; b wins
		 (let ((top-a (pop a))
		       (top-b (pop b)))
		   (if b (progn (push top-b (cdr (last b)))
				(push top-a (cdr (last b)))
				(values a b))
		       (values a (list top-b top-a))))))

	   (winner (a b)
	     (let ((seen (make-hash-table :test 'equal)))
	       (loop while (and a b)
		     when (gethash (cons a b) seen)
		       return (values 1 a)

		     finally (return (if a (values 1 a) (values 2 b)))

		     do (setf (gethash (cons a b) seen) t)
			(if (and (<= (car a) (1- (length a)))
				 (<= (car b) (1- (length b))))

			    (multiple-value-setq (a b)
			      (forth-normal (copy-seq a) (copy-seq b)
					    (winner (subseq (cdr a) 0 (car a))
						    (subseq (cdr b) 0 (car b)))))

			    (multiple-value-setq (a b)
			      (forth-normal (copy-seq a) (copy-seq b))))))))

    (loop for card in (reverse (nth-value 1 (winner (first data)
						    (second data))))
	  and worth from 1
	  sum (* card worth))))
