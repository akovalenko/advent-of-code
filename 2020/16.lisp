(in-package #:advent-of-code.2020.16)

;;; Advent of code 2020: day 16
;;; see https://adventofcode.com/2020/day/16

(defun parse-input ()
  (line-groups (file-lines (my-input-path) :remove-empty nil)))

(defun parse-ranges (line)
  (let* ((ranges (second (split-sequence #\: line)))
	 (or-at (search " or " ranges))
	 (range1 (subseq ranges 0 or-at))
	 (range2 (subseq ranges (+ or-at (length " or ")))))
    (flet ((parse-range (r)
	     (mapcar 'parse-integer (split-sequence #\- r))))
      (list (parse-range range1)
	    (parse-range range2)))))

(defun part-1 (&optional (data (parse-input)))
  (let* ((valid-ranges (mapcar 'parse-ranges (first data)))
	 (nearby-tickets (third data))
	 (nearby-tickets
	   (mapcar (lambda (nt)
		     (mapcar 'parse-integer (split-sequence #\, nt)))
		   (rest nearby-tickets))))

    (flet ((valid-for-some-field (value)
	     (dolist (vr valid-ranges)
	       (destructuring-bind ((low1 high1) (low2 high2)) vr
		 (when (or (<= low1 value high1)
			   (<= low2 value high2))
		   (return-from valid-for-some-field t))))))
      (loop for nt in nearby-tickets
	    sum (loop for field in nt
		      unless (valid-for-some-field field)
			sum field)))))

(defun parse-ranges-v2 (line)
  (let* ((split  (split-sequence #\: line))
	 (header (first split))
	 (ranges (second split))
	 (or-at (search " or " ranges))
	 (range1 (subseq ranges 0 or-at))
	 (range2 (subseq ranges (+ or-at (length " or ")))))
    (flet ((parse-range (r)
	     (mapcar 'parse-integer
		     (split-sequence:split-sequence #\- r))))
      (list header
	    (parse-range range1)
	    (parse-range range2)))))

(defun part-2 (&optional (data (parse-input)))
  (let* ((valid-ranges (mapcar 'parse-ranges-v2 (first data)))
	 (your-ticket (second (second data)))
	 (nearby-tickets (third data))
	 (nearby-tickets
	   (mapcar (lambda (ticket)
		     (mapcar 'parse-integer (split-sequence #\, ticket)))
		   (rest nearby-tickets))))

    (flet ((valid-for-some-field (value)
	     (dolist (valid-range valid-ranges)
	       (destructuring-bind (header (low1 high1) (low2 high2))
		   valid-range
		 (declare (ignore header))
		 (when (or (<= low1 value high1)
			   (<= low2 value high2))
		   (return-from valid-for-some-field t))))))
      ;; remove invalid tickets
      (setf nearby-tickets
	    (loop for nt in nearby-tickets
		  when (loop for field in nt
			     always (valid-for-some-field field))
		    collect nt))

      (let ((all-fields (mapcar 'first valid-ranges))
	    (field-count (length (first nearby-tickets))))
	(let ((pf-map
		(loop for i below field-count
		      for possible-fields = (copy-seq all-fields)
		      do (dolist (nearby nearby-tickets)
			   (let ((value (elt nearby i)))
			     (dolist (field possible-fields)
			       (destructuring-bind ((low1 high1) (low2 high2))
				   (rest (assoc field valid-ranges
						:test 'string=))
				 (unless (or (<= low1 value high1)
					     (<= low2 value high2))
				   (setf possible-fields
					 (remove field possible-fields
						 :test 'string=))
				   (assert possible-fields))))))
		      collect possible-fields)))

	  (let ((field-dict
		  (loop for singular = (position 1 pf-map :key 'length)
			while singular
			for name = (car (shiftf (elt pf-map singular) nil))
			collect (cons singular name)
			do (setf pf-map (mapcar
					 (lambda (list)
					   (remove name list :test 'string=))
					 pf-map)))))

	    (let ((your-ticket
		    (mapcar 'parse-integer
			    (split-sequence:split-sequence #\, your-ticket))))
	      (apply '*
		     (loop for (n . field) in field-dict
			   when (alexandria:starts-with-subseq
				 "departure" field)
			     collect (elt your-ticket n))))))))))
