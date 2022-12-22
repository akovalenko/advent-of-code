(in-package #:advent-of-code.2022.21)

;;; Advent of code 2022: day 21
;;; see https://adventofcode.com/2022/day/21

(defun parse-line (line)
  (let* ((colon (position #\: line))
	 (defun (subseq line 0 colon))
	 (rest (subseq line (+ 2 colon))))
    (if-let ((n (parse-integer rest :junk-allowed t)))
      (list defun n)
      (destructuring-bind (a op b) (split-sequence #\Space rest)
	(list defun (list (find-symbol op :cl) a b))))))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)))
  (let ((resolved (make-hash-table :test 'equal))
	(formulas (make-hash-table :test 'equal)))
    (loop for (name defn) in data
	  when (integerp defn)
	    do (setf (gethash name resolved) defn)
	  else
	    do (setf (gethash name formulas) defn))
    (labels ((find-out (what)
	       (or
		(gethash what resolved)
		(destructuring-bind (op a b) (gethash what formulas)
		  (setf (gethash what resolved)
			(funcall op (find-out a) (find-out b)))))))
      (find-out "root"))))


(defun part-2 (&optional (data (parse-input)))
  (let ((resolved (make-hash-table :test 'equal))
	(formulas (make-hash-table :test 'equal)))
    (loop for (name defn) in data
	  when (equal "root" name)
	    do (setf defn (list* '- (rest defn)))
	  when (integerp defn)
	    do (setf (gethash name resolved) defn)
	  else
	    do (setf (gethash name formulas) defn))
    (setf (gethash "humn" resolved) :var)
    (labels ((xfuncall (op a b)
	       (cond
		 ((or (eq a :var) (eq b :var)) :var)
		 (t (funcall op a b))))
	     (find-out (what)
	       (or
		(gethash what resolved)
		(destructuring-bind (op a b) (gethash what formulas)
		  (setf (gethash what resolved)
			(xfuncall op (find-out a) (find-out b)))))))
      (labels ((equalize (symbol value)
		 (let ((formula (gethash symbol formulas)))
		   (unless formula
		     (assert (string= "humn" symbol))
		     (return-from part-2 value))
		   (destructuring-bind (op a b) formula
		     (let ((fa (find-out a))
			   (fb (find-out b)))
		       (assert (some 'numberp (list fa fb)))
		       (when (every 'numberp (list fa fb))
			 (assert (= value (funcall op fa fb)))
			 (return-from part-2 :vanished))
		       (if (eq :var fa) ;; x op fb = value
			   (equalize a
				   (ecase op
				     (+ (- value fb))
				     (- (+ value fb))
				     (* (/ value fb))
				     (/ (* value fb))))
			   (equalize b ;; fa op x = value
				   (ecase op
				     (+ (- value fa))
				     (- (- fa value))
				     (* (/ value fa))
				     (/ (/ fa value))))))))))
	(equalize "root" 0)))))
