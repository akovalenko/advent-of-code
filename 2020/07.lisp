(in-package #:advent-of-code.2020.07)

;;; Advent of code 2020: day 07
;;; see https://adventofcode.com/2020/day/7

(defun parse-input ()
  (flet ((parse-element (element)
	   (multiple-value-bind (n next)
	       (parse-integer element :junk-allowed t)
	     (and n (list n (string-trim '(#\Space) (subseq element next (position #\Space element :from-end t))))))))
   (loop with hash-table = (make-hash-table :test 'equal)
	 for line in (file-lines (my-input-path))
	 for bags-contain = (search #1="bags contain" line)
	 for colour = (subseq line 0 (1- bags-contain))
	 for elements = (split-sequence #\, (subseq line (+ bags-contain (length #1#) 1) (1- (length line))))
	 do (setf (gethash colour hash-table) (remove nil (mapcar #'parse-element elements)))
	 finally (return hash-table))))

(defun part-1 (&optional (data (parse-input)))
  (let ((bag-containers (make-hash-table :test 'equal)))
    (loop for colour being the hash-key of data
	    using (hash-value elements)
	  do (loop for (nil item) in elements
		   do (pushnew colour (gethash item bag-containers)
			       :test 'equal)))
    (let ((possible-outermost (make-hash-table :test 'equal)))
      (labels ((frob (colour)
		 (dolist (outer (gethash colour bag-containers))
		   (setf (gethash outer possible-outermost) t)
		   (frob outer))))
	(frob "shiny gold")
	(hash-table-count possible-outermost)))))

(defun part-2 (&optional (data (parse-input)))
  (let ((cache (make-hash-table :test 'equal)))
    (labels ((inner-bag-count (colour)
	       (or (gethash colour cache)
		   (setf (gethash colour cache)
			 (loop for (n item) in (gethash colour data)
			       sum (* n (1+ (inner-bag-count item))))))))
      (inner-bag-count "shiny gold"))))
