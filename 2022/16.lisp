(in-package #:advent-of-code.2022.16)

;;; Advent of code 2022: day 16
;;; see https://adventofcode.com/2022/day/16

(defun parse-line (line)
  (let* ((words (split-sequence #\Space line))
	 (valve (elt words 1))
	 (rate (let ((assign (elt words 4)))
		 (parse-integer assign :start (1+ (position #\= assign))
				       :junk-allowed t)))
	 (tunnels (loop for tun in (subseq words 9)
			collect (string-right-trim '(#\,) tun))))
    (list valve rate tunnels)))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))


(defun distances (data)
  (let ((fw (make-hash-table :test 'equal)))
    (loop for (name) in data
	  do (setf (gethash (list name name) fw) 0))
    (loop for (name _ neighs) in data
	  do (loop for neigh in neighs
		   do (setf (gethash (list name neigh) fw) 1)))
    (loop for (k) in data
	  do (loop for (i) in data
		   do (loop for (j) in data
			    for d-ik = (gethash (list i k) fw)
			    for d-kj = (gethash (list k j) fw)
			    for d-ij = (gethash (list i j) fw)
			    when (and d-ik
				      d-kj
				      (or (null d-ij)
					  (> d-ij (+ d-ik d-kj))))
			      do (setf (gethash (list i j) fw)
				       (+ d-ik d-kj)))))
    fw))

(defun makes-sense-to-open (data)
  (mapcar 'first (remove-if-not 'plusp data :key 'second)))

(defun best-score (start distances where data)
  (let ((best (fourth start)))
    (loop for next in (set-difference where (third start) :test 'equal)
	  ;; going there and opening
	  for budget = (1+ (gethash (list (second start) next) distances))
	  when (< budget (first start))
	    do (setf best
		     (max best
			  (best-score
			   (list (- (first start) budget)
				 next
				 (cons next (third start))
				 (+ (fourth start)
				    (* (- (first start) budget)
				       (second (assoc next data :test 'equal)))))
			   distances where data))))
    best))


;;; rewrite of best-score using closed valve set instead of open+full
;;; valve sets (avoiding set-intersection)
(defun best-score-rvf (start distances where data)
  (let ((best (fourth start)))
    (loop for next in (third start)
	  ;; going there and opening
	  for budget = (1+ (gethash (list (second start) next) distances))
	  when (< budget (first start))
	    do (setf best
		     (max best
			  (best-score-rvf
			   (list (- (first start) budget)
				 next
				 (remove next (third start))
				 (+ (fourth start)
				    (* (- (first start) budget)
				       (second (assoc next data :test 'equal)))))
			   distances where data))))
    best))


(defun part-1 (&optional (data (parse-input)))
  ;; state: how many minutes left, where I am, which valves are closed, secured score
  ;; 30 "AA" (...) 0
  (let* ((distances (distances data))
	 (where (makes-sense-to-open data))
	 ;; budget whereami openset score
	 (state (list 30 "AA" where 0)))
    (best-score-rvf state distances where data)))

(defun part-2 (&optional (data (parse-input)))
  (let* ((distances (distances data))
	 (where (makes-sense-to-open data))
	 ;; budget whereami openset score
	 (partitions (expt 2 (1- (length where)))))
    (loop for part below partitions
	  for mine = (loop for target in where
			   for bit from 0
			   when (logbitp bit part)
			     collect target)
	  for eles = (set-difference where mine :test 'equal)
	  do (print part)
	  maximize (+ (best-score-rvf (list 26 "AA" mine 0) distances where data)
		      (best-score-rvf (list 26 "AA" eles 0) distances where data)))))


;; approximate rewrite of the above for speed (bit fiddling,
;; numeric-only data structures)

(defun distance-matrix-and-power-vector (data)
  (let* ((hash (distances data))
	 (anchors (cons "AA" (remove "AA" (makes-sense-to-open data) :test 'equal)))
	 (dist (make-array (list (length anchors) (length anchors)) :initial-element nil))
	 (pow (make-array (list (length anchors)))))
    (loop for i from 0
	  for ai in anchors
	  do (setf (aref pow i)
		   (second (assoc ai data :test 'equal)))
	  do (loop for j from 0
		   for aj in anchors
		   do (setf (aref dist i j)
			    (gethash (list ai aj) hash))))
    (values dist pow)))

(defun best-score-fast (budget location closed score dm pv)
  (let ((best score))
    (loop for next from 1 below (integer-length closed)
	  for spent = (1+ (aref dm location next))
	  when (and (logbitp next closed)
		    (> budget spent))
	    do (setf best (max best
			       (best-score-fast (- budget spent)
						next
						(logxor closed (ash 1 next))
						(+ score
						   (* (- budget spent)
						      (aref pv next)))
						dm
						pv)))
	  finally (return best))))

(defun part-1-fast (&optional (data (parse-input)))
  (multiple-value-bind (dm pv) (distance-matrix-and-power-vector data)
    (let ((all-closed (- (expt 2 (length pv)) 2)))
      (best-score-fast 30 0 all-closed 0 dm pv))))

(defun part-2-fast (&optional (data (parse-input)))
  (multiple-value-bind (dm pv) (distance-matrix-and-power-vector data)
    (let ((all-closed (- (expt 2 (length pv)) 2)))
      (loop for partition below (expt 2 (- (length pv) 2))
	    for mine = (ash partition 1)
	    for eles = (logxor all-closed (ash partition 1))
	    when (zerop (mod partition 100)) do (print partition)
	    maximize (+ (best-score-fast 26 0 mine 0 dm pv)
			(best-score-fast 26 0 eles 0 dm pv))))))
