(in-package :advent-of-code.2020.19)

(defun parse-input ()
  (destructuring-bind (rules abs)
      (line-groups
       (file-lines (my-input-path) :remove-empty nil))
    (list (mapcar #'parse-rule rules) abs)))

(defun parse-rule (rule)
  (destructuring-bind (left right)
      (split-sequence #\: rule :count 2)
    (let ((left (parse-integer left))
	  (right (split-sequence #\| right)))
      (list left (mapcar (lambda (seq)
			   (read-from-string (str "(" seq ")")))
			 right)))))

(assert (equal (parse-rule "25: 6 54 | 28 122")
	       '(25 ((6 54) (28 122)))))

(defun part-1 (&optional (data (parse-input)))
  (let ((rule-table (make-hash-table))
	(regexp-table (make-hash-table)))
    (destructuring-bind (rules abs) data

      (loop for (id expansions) in rules
	    do (setf (gethash id rule-table) expansions))

      (labels ((rule-regexp (n)
		 (or (gethash n regexp-table)
		     (setf (gethash n regexp-table)
			   (let ((expansion (gethash n rule-table)))
			     (cond
			       ((stringp (first (first expansion)))
				(first (first expansion)))
			       (t
				(str "(?:"
				     (apply 'str (rest
						  (loop for possible in expansion
							collect "|"
							nconc (loop for deep in possible
								    collect (rule-regexp deep)))))
				     ")"))))))))

	(let ((scanner (ppcre:create-scanner (str "^" (rule-regexp 0) "$"))))
	  (loop for ab in abs
		count (ppcre:scan scanner ab)))))))


(defun part-2 (&optional (data (parse-input)))
  (let ((rule-table (make-hash-table))
	(regexp-table (make-hash-table)))
    (destructuring-bind (rules abs) data

      (loop for (id expansions) in rules
	    do (setf (gethash id rule-table) expansions))

      (let ((plus-hack (caar (gethash 0 rule-table)))
	    (balance-hack (cadar (gethash 0 rule-table))))
	(labels ((rule-regexp (n)
		   (or (gethash n regexp-table)
		       (setf (gethash n regexp-table)
			     (let ((expansion (gethash n rule-table)))
			       (if (= n balance-hack)
				   (destructuring-bind (first second) (first expansion)
				     (with-output-to-string (output)
				       (write-string "(?:" output)
				       (loop for repeat from 1 below 6
					     for or = nil then t
					     do (when or
						  (write-string "|" output))
						(write-string "(?:" output)
						(write-string (rule-regexp first) output)
						(format output "{~a}" repeat)
						(write-string (rule-regexp second) output)
						(format output "{~a}" repeat)
						(write-string ")" output))
				       (write-string ")" output)))
				   (cond
				     ((stringp (first (first expansion)))
				      (first (first expansion)))
				     (t
				      (str "(?:"
					   (apply 'str (rest
							(loop for possible in expansion
							      collect "|"
							      nconc (loop for deep in possible
									  collect (rule-regexp deep)))))
					   ")"
					   (if (= n plus-hack) "+" ""))))))))))

	  (let ((scanner (ppcre:create-scanner (str "^" (rule-regexp 0) "$"))))
	    (loop for ab in abs
		  count (ppcre:scan scanner ab))))))))
