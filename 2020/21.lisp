(in-package #:advent-of-code.2020.21)

;;; Advent of code 2020: day 21
;;; see https://adventofcode.com/2020/day/21

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun parse-line (line)
  (destructuring-bind (who contains) (split-sequence #\( line)
    (let ((who (split-sequence #\Space who :remove-empty-subseqs t))
	  (contains (subseq contains (length "contains ") (1- (length contains)))))
      (setf contains
	    (mapcar (curry 'string-trim '(#\Space))
		    (split-sequence #\, contains)))
      (list who contains))))

(defun part-1 (&optional (data (parse-input)))
  (let* ((allergens (remove-duplicates (reduce 'append (mapcar 'second data))
				       :test 'string=))
	 (foods (remove-duplicates (reduce 'append (mapcar 'first data))
				   :test 'string=))
	 (map (make-array (list (length allergens)
				(length foods))
			  :element-type 'bit :initial-element 1)))

    (labels ((aindex (a) (position a allergens :test 'string=))
	     (findex (f) (position f foods :test 'string=)))
      (let ((rules (loop for (foods poisons) in data
			 collect (list (mapcar #'findex foods)
				       (mapcar #'aindex poisons)))))

	(loop for (food-list poisons) in rules
	      do (dotimes (fx (length foods))
		   (unless (find fx food-list)
		     (dolist (ax poisons)
		       (setf (aref map ax fx) 0)))))

	(dotimes (ax (length allergens))
	  (format *debug-io* "~&~a may be in:~&" (elt allergens ax))
	  (dotimes (fx (length foods))
	    (when (= 1 (aref map ax fx))
	      (format *debug-io* "~& ~a~&" (elt foods fx)))))

	(loop for fx below (length foods)
	      when (loop for ax below (length allergens)
			 always (= 0 (aref map ax fx)))
		sum (loop for (foods poisons) in rules
			  sum (count fx foods)))))))

(defun part-2 (&optional (data (parse-input)))
  (let* ((allergens (remove-duplicates (reduce 'append (mapcar 'second data))
				       :test 'string=))
	 (foods (remove-duplicates (reduce 'append (mapcar 'first data))
				   :test 'string=))
	 (map (make-array (list (length allergens)
				(length foods))
			  :element-type 'bit :initial-element 1)))

    (labels ((aindex (a) (position a allergens :test 'string=))
	     (findex (f) (position f foods :test 'string=)))
      (let ((rules (loop for (foods poisons) in data
			 collect (list (mapcar #'findex foods)
				       (mapcar #'aindex poisons)))))

	(loop for (food-list poisons) in rules
	      do (dotimes (fx (length foods))
		   (unless (find fx food-list)
		     (dolist (ax poisons)
		       (setf (aref map ax fx) 0)))))

	(dotimes (ax (length allergens))
	  (format *debug-io* "~&~a may be in:~&" (elt allergens ax))
	  (dotimes (fx (length foods))
	    (when (= 1 (aref map ax fx))
	      (format *debug-io* "~& ~a~&" (elt foods fx)))))

	(loop with unsolved-allergens = (loop for ax below (length allergens) collect ax)
	      with result = nil
	      while unsolved-allergens

	      do (dolist (ax unsolved-allergens)
		   (let ((food-list (loop for fx below (length foods)
					  when (= 1 (aref map ax fx))
					    collect fx)))
		     (when (null (cdr food-list))
		       (format *debug-io* "~&solved ~a~&" ax)
		       (setf unsolved-allergens (remove ax unsolved-allergens))
		       (push (cons ax (car food-list)) result)


		       (dotimes (ax (length allergens))
			 (format *debug-io* "~&~a may be in:~&" (elt allergens ax))
			 (dotimes (fx (length foods))
			   (when (= 1 (aref map ax fx))
			     (format *debug-io* "~& ~a~&" (elt foods fx)))))

		       (dotimes (fx (length foods))
			 (unless (= fx (car food-list))
			   (setf (aref map ax fx) 0)))

		       (dotimes (other-ax (length allergens))
			 (unless (= other-ax ax)
			   (setf (aref map other-ax (car food-list)) 0)))


		       )))
	      finally (return
			(subseq
			 (with-output-to-string (output)
			   (dolist (fx
				    (mapcar 'cdr
					    (sort result (lambda (a b)
							   (string< (elt allergens (car a))
								    (elt allergens (car b)))))))
			     (write-string "," output)
			     (write-string (elt foods fx) output)))
			 1)))))))
