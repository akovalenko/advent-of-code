(in-package #:advent-of-code.2021.19)

;;; Advent of code 2021: day 19
;;; see https://adventofcode.com/2021/day/19

(defun parse-triplet (string)
  (mapcar 'parse-integer (split-sequence #\, string)))

(defun parse-input ()
  (mapcar (lambda (list) (mapcar 'parse-triplet list))
	  (mapcar 'rest
		  (line-groups
		   (file-lines (my-input-path) :remove-empty nil)))))

;;;; cube motion algebra

;;; Each cube rotation is a composition of flipping some axes and
;;; permuting some coordinates, with an additional restriction that
;;; only odd permutations are considered when odd number of axes is
;;; flipped, and only even permutations are considered when even
;;; number of axes is flipped.
;;;
;;; In order to get transformed coordinates, first flipping then
;;; permutation should be applied to old coordinates.

(defstruct cube-rotation
  (permute #(0 1 2) :type simple-vector)
  (flip #(1 1 1) :type simple-vector))

(defun gen-cube-flips ()
  (map 'simple-vector
       (lambda (bits)
	 (map 'simple-vector
	      (lambda (bit)
		(if (logbitp bit bits) -1 1))
	      '(0 1 2)))
       (loop for i below 8 collect i)))

(defun even-flip-p (flip)
  (evenp (count -1 flip)))

(defun gen-permutations ()
  (let (even odd half)
    (map-permutations
     (lambda (perm)
       (if (setf half (not half))
	   (push perm even)
	   (push perm odd)))
     #(0 1 2))
    (values (nreverse even)
	    (nreverse odd))))

(defun gen-cube-rotations ()
  (let ((result (make-sequence 'simple-vector 24)))
    (multiple-value-bind (even odd)
	(gen-permutations)
      (loop for flip across (gen-cube-flips)
	    for flip-index below 8
	    do (loop for perm in (if (even-flip-p flip) even odd)
		     for perm-index below 3
		     do (setf (svref result (+ (* flip-index 3) perm-index))
			      (make-cube-rotation :permute perm
						  :flip flip)))))
    result))

(defun apply-flip (flip vector)
  (map 'vector '* flip vector))

(defun apply-perm (perm vector)
  (map 'vector (lambda (pos)
		 (svref vector pos))
       perm))

(defun cube-rotation-number (rotation)
  (destructuring-bind (even odd)
      (load-time-value (multiple-value-list (gen-permutations)))
    (loop for flip across (cube-rotation-flip rotation)
	  for bit-position from 0
	  with flip-evenp = t
	  when (= -1 flip)
	    sum (ash 1 bit-position) into flip-index
	    and do (setf flip-evenp (not flip-evenp))
	  finally (return
		    (+ (* flip-index 3)
		       (position (cube-rotation-permute rotation)
				 (if flip-evenp even odd)
				 :test 'equalp))))))

(defun check-cube-rotation-numbering ()
  (let ((rotations (load-time-value (gen-cube-rotations))))
    (loop for rot across rotations
	  for index from 0
	  do (assert (= index (cube-rotation-number rot))))))

(defun invert-perm (vec)
  (let* ((length (length vec))
	 (inverted (make-sequence 'simple-vector length)))
    (dotimes (i length)
      (setf (svref inverted (svref vec i)) i))
    inverted))

(defun invert-rotation (rotation)
  (make-cube-rotation
   :permute (invert-perm (cube-rotation-permute rotation))
   :flip (apply-perm (cube-rotation-permute rotation)
		     (cube-rotation-flip rotation))))

(defun describe-cube-rotation (n)
  (svref (load-time-value (gen-cube-rotations)) n))

(defun compose-cube-rotations (first second)
  ;; flip1 perm1 flip2 perm2 ==
  ;; flip1 (unpermuted perm1 flip2) perm1 perm2
  (make-cube-rotation
   :permute (apply-perm (cube-rotation-permute second)
			(cube-rotation-permute first))
   :flip (apply-flip (cube-rotation-flip first)
		     (apply-perm (invert-perm (cube-rotation-permute first))
				 (cube-rotation-flip second)))))

;;; "Public" API doesn't operate on cube-rotation structures, using
;;; only rotation IDs instead

(defun cr-apply (n point)
  (let ((rot (describe-cube-rotation n)))
    (apply-perm
     (cube-rotation-permute rot)
     (apply-flip (cube-rotation-flip rot) point))))

(defun cr-compose (first second)
  (aref (load-time-value
	 (let ((table (make-array '(24 24))))
	   (dotimes (i 24 table)
	     (dotimes (j 24)
	       (setf (aref table i j)
		     (cube-rotation-number
		      (compose-cube-rotations
		       (describe-cube-rotation i)
		       (describe-cube-rotation j))))))))
	first second))

(defun cr-invert (n)
  (svref (load-time-value
	  (let ((table (make-sequence 'simple-vector 24)))
	    (dotimes (i 24)
	      (setf (svref table i)
		    (cube-rotation-number
		     (invert-rotation
		      (describe-cube-rotation i)))))
	    table))
	 n))

(defun check-cube-rotation-ops ()
  (dotimes (i 24)
    (assert (= 0 (cr-compose i (cr-invert i))
	       (cr-compose (cr-invert i) i))))
  (dotimes (i 24)
    (dotimes (j 24)
      (dotimes (k 24)
	(assert (= (cr-compose (cr-compose i j) k)
		   (cr-compose i (cr-compose j k))))))))

;;; now we're ready to define cube motions, each motion being composed
;;; of adding a vector and THEN applying rotation.

(defstruct (moved-cube-rotation (:type vector))
  (offset #(0 0 0))
  (ncr 0))

(defun mcr-apply (mcr vector)
  (cr-apply
   (moved-cube-rotation-ncr mcr)
   (map 'vector '+ vector (moved-cube-rotation-offset mcr))))

(defun mcr-invert (mcr)
  (let ((ncr (moved-cube-rotation-ncr mcr))
	(off (moved-cube-rotation-offset mcr)))
    (make-moved-cube-rotation
     :ncr (cr-invert ncr)
     :offset (cr-apply ncr (map 'vector '- off)))))

(defun mcr-compose (first second)
  (make-moved-cube-rotation
   :ncr (cr-compose (moved-cube-rotation-ncr first)
		    (moved-cube-rotation-ncr second))
   :offset (map 'vector '+
		(moved-cube-rotation-offset first)
		(cr-apply (cr-invert (moved-cube-rotation-ncr first))
			  (moved-cube-rotation-offset second)))))

(defun check-mcr-ops ()
  (let ((base #(123 848 383))
	(base2 #(381 22 33))
	(start #(28 123 81)))
    (dotimes (r1 24)
      (let ((op (make-moved-cube-rotation :offset base :ncr r1)))
	(assert (equalp start
			(mcr-apply (mcr-invert op)
				   (mcr-apply op start))))
	(dotimes (r2 24)
	  (let ((op2 (make-moved-cube-rotation :offset base2 :ncr r2)))
	    (assert (equalp (mcr-apply op2 (mcr-apply op start))
			    (mcr-apply (mcr-compose op op2) start)))))))))

(defun make-diff-hash (point-vector)
  (let ((hash-table (make-hash-table :test 'equalp))
	(length (length point-vector)))
    (dotimes (i length hash-table)
      (dotimes (j length)
	(unless (= i j)
	  (let ((diff (map 'vector '-
			   (svref point-vector i)
			   (svref point-vector j))))
	    (dotimes (rot 24)
	      (push (list rot i j)
		    (gethash (cr-apply rot diff) hash-table)))))))))

(defstruct (scan-table (:type vector))
  (points #() :type simple-vector)
  (diff-hash nil :type hash-table)
  (point-hash nil :type hash-table))

(defun create-scan-table (points)
  (let ((points (coerce (mapcar (curry 'apply 'vector) points)
			'simple-vector)))
    (make-scan-table
     :points points
     :diff-hash (make-diff-hash points)
     :point-hash (alist-hash-table
		  (loop for point across points
			and index from 0
			collect (cons point index))
		  :test 'equalp))))

(defun visible-p (point)
  (every '<=
	 #(-1000 -1000 -1000)
	 point
	 #(1000 1000 1000)))

(defun verify-location (first second mcr)
  (let ((second-point-hash (scan-table-point-hash second)))
    (<= 12 (loop for point across (scan-table-points first)
		 for xformed = (mcr-apply mcr point)
		 when (gethash xformed second-point-hash)
		   count t
		 else
		   when (visible-p xformed)
		     return 0))))

(defun try-match-scan-tables (first second)
  (let ((first-points (scan-table-points first))
	(second-diffs (scan-table-diff-hash second))
	(second-points (scan-table-points second)))
    (let ((pt1 (random-elt first-points))
	  (pt2 (random-elt first-points)))
      (unless (eq pt1 pt2)
	(loop
	  with delta = (map 'vector '- pt1 pt2)
	  for (rot i j) in (gethash delta second-diffs)
	  for unrot = (cr-invert rot)
	  
	  ;; we know rot(second#i)-rot(second#j) = (pt1-pt2)
	  ;; we assume rot(second#i) matches pt1

	  for offset = (map 'vector '- (cr-apply rot (svref second-points i))
			    pt1)
	  
	  for mcr = (make-moved-cube-rotation :offset offset :ncr unrot)
	  when (verify-location first second mcr)
	    do (return (mcr-invert mcr)))))))

(defun locate-scanners (data)
  (let* ((scan-tables (map 'simple-vector 'create-scan-table data))
	 (length (length scan-tables))
	 (locations (make-sequence 'simple-vector length :initial-element nil)))
    (setf (svref locations 0) (make-moved-cube-rotation))
    (loop
      named discovery
      do (loop for location across locations
	       for index from 0
	       when location
		 collect index into knowns
	       else
		 collect index into unknowns
	       finally
		  (unless unknowns
		    (return-from discovery))
		  (let* ((known (random-elt knowns))
			 (candidate (random-elt unknowns)))
		    (let ((mcr (try-match-scan-tables
				(svref scan-tables known)
				(svref scan-tables candidate))))
		      (when mcr
			(setf (svref locations candidate)
			      (mcr-compose mcr
					   (svref locations known))))))))
    (values scan-tables locations)))

(defun part-1 (&optional (data (parse-input)))
  (multiple-value-bind (scan-tables locations)
      (locate-scanners data)
    (loop with hash-table = (make-hash-table :test 'equalp)
	  for mcr across locations
	  and scan-table across scan-tables
	  do (loop for point across (scan-table-points scan-table)
		   for xformed = (mcr-apply mcr point)
		   do (setf (gethash xformed hash-table) t))
	  finally (return (hash-table-count hash-table)))))

(defun manhattan-distance (pt1 pt2)
  (reduce '+ (map 'vector '- pt1 pt2) :key 'abs))

(defun part-2 (&optional (data (parse-input)))
  (multiple-value-bind (scan-tables locations)
      (locate-scanners data)
    (declare (ignorable scan-tables))
    (let ((scanner-places
	    (loop for mcr across locations
		  collect (mcr-apply mcr #(0 0 0)))))
      (loop for a in scanner-places
	    maximize (loop for b in scanner-places
			   maximize (manhattan-distance a b))))))

