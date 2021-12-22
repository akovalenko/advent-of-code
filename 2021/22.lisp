(in-package #:advent-of-code.2021.22)

;;; Advent of code 2021: day 22
;;; see https://adventofcode.com/2021/day/22

(defun parse-line (line)
  (destructuring-bind (onoff ranges)
      (split-sequence #\Space line)
    (list (string= onoff "on")
	  (loop for range in (split-sequence #\, ranges)
		for dots = (search ".." range)
		collect (cons (parse-integer range :start 2 :end dots)
			      (parse-integer range :start (+ 2 dots)))))))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun model-small-area (data)
  (let ((bits (make-array '(101 101 101) :element-type 'bit :initial-element 0)))
    (flet ((frob-pair (cons)
	     (let ((v0 (car cons))
		   (v1 (cdr cons)))
	       (list (+ 50 (max (min v0 v1) -50))
		     (+ 50 (min (max v0 v1) 50))))))
      (loop for (turn-on (xs ys zs)) in data
	    do (loop
		 with (xa xb) = (frob-pair xs)
		 with (ya yb) = (frob-pair ys)
		 with (za zb) = (frob-pair zs)
		 for x from xa to xb
		 do (loop for y from ya to yb
			  do (loop for z from za to zb
				   do (setf (aref bits x y z)
					    (if turn-on 1 0)))))
	    finally (return (count 1 (flat-array-alias bits)))))))

(defun key-points (data axis)
  (flet ((frob-pair (cons)
	   (let ((v0 (car cons))
		 (v1 (cdr cons)))
	     (list (min v0 v1)
		   (1+ (max v0 v1))))))
    (alist-hash-table
     (loop for key in (sort
		       (remove-duplicates
			(loop for (turn-on pairs) in data
			      nconc (frob-pair (elt pairs axis))))
		       #'<)
	   for index from 0
	   collect (cons key index)))))

(defun all-key-points (data)
  (loop for axis below 3
	collect (key-points data axis)))

(defun kp-weights (kp)
  (let ((rev-kp
	  (alist-hash-table
	   (mapcar (lambda (pair)
		     (cons (cdr pair) (car pair)))
		   (hash-table-alist kp)))))
    (coerce
     (loop for index from 0
	   for this = (gethash index rev-kp)
	   and next = (gethash (1+ index) rev-kp)
	   while next
	   collect (- next this))
     'simple-vector)))

(defun model-whole-area (data)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((kp (all-key-points data))
	 (dimensions (mapcar 'hash-table-count kp))
	 (bits (make-array dimensions :element-type 'bit
				      :initial-element 0)))
    (flet ((frob-pair (cons)
	     (let ((v0 (car cons))
		   (v1 (cdr cons)))
	       (list (min v0 v1)
		     (max v0 v1)))))
      (loop for (turn-on ranges) in data
	    for ((xa . xb) (ya . yb) (za . zb))
	      = (loop for range in ranges
		      for kp-table in kp
		      for (v0 v1) = (frob-pair range)
		      collect (cons (gethash v0 kp-table)
				    (gethash (1+ v1) kp-table)))
	    do (loop with bit = (if turn-on 1 0)
		     for x from xa below xb
		     do (loop for y from ya below yb
			      do (loop for z from za below zb
				       do (setf (aref bits x y z) bit))))
	    finally
	       (return-from model-whole-area
		 (loop with (w0 w1 w2) = (mapcar 'kp-weights kp)
		       for wx across w0
		       for x from 0 below (length w0)
		       sum (* wx (loop for y from 0 below (length w1)
				       for wy across w1
				       sum (* wy (loop for z from 0
							 below (length w2)
						       for wz across w2
						       when (= 1 (aref bits x y z))
							 sum wz))))))))))

(defun part-1 (&optional (data (parse-input)))
  (model-small-area data))

(defun part-2 (&optional (data (parse-input)))
  (model-whole-area data))

(defun consort (cons)
  (if (<= (car cons) (cdr cons))
      cons (cons (cdr cons) (car cons))))

(defun rev-kp-get (n hash-table)
  (maphash (lambda (k v)
	     (when (= v n)
	       (return-from rev-kp-get k)))
	   hash-table))

(defun propose-partition (data axis)
  (let* ((kp (key-points data axis))
	 (nkps (hash-table-count kp))
	 (starts
	   (make-sequence 'simple-vector nkps :initial-element 0))
	 (ends
	   (make-sequence 'simple-vector nkps :initial-element 0)))
    (loop for (onoff ranges) in data
	  for (a . b) = (consort (elt ranges axis))
	  do (incf (svref starts (gethash a kp)))
	     (incf (svref ends (gethash (1+ b) kp))))
    (loop with ended = 0
	  for ending-here across ends
	  for index from 0
	  do (incf ended ending-here)
	     (setf (svref ends index) ended))
    (loop with started = 0
	  for index downfrom (1- nkps) to 0
	  for starting-here = (svref starts index)
	  do (incf started starting-here)
	     (setf (svref starts index) started))
    (loop
      with all = (svref starts 0)
      with best-cost = (svref starts 0) and best-split = 0
      for split below nkps
      for lefts = (svref ends split)
      for rights = (svref starts split)
      for crossers = (- all lefts rights)
      for cost = (+ crossers (max lefts rights))
      when (< cost best-cost)
	do (setf best-cost cost
		 best-split split)
      finally (return (values best-cost (rev-kp-get best-split kp))))))

(defun propose-partition-axis (data)
  (loop for axis below 3
	for best-cost = (1- (length data))
	for best-value = nil
	for best-axis = nil
	do (multiple-value-bind (cost value)
	       (propose-partition data axis)
	     (when (< cost best-cost)
	       (setf best-cost cost
		     best-value value
		     best-axis axis)))
	finally (return (values best-axis best-value best-cost))))

(defun partition (data axis value)
  (loop with left = nil and right = nil
	for item in data
	for (onoff ranges) = item
	for (v0 . v1) = (consort (elt ranges axis))
	do (cond
	     ((< v1 value)
	      (push item left))
	     ((<= value v0)
	      (push item right))
	     (t ;; split
	      (let ((spec-left (cons v0 (1- value)))
		    (spec-right (cons value v1))
		    (ranges-left (copy-list ranges))
		    (ranges-right (copy-list ranges)))
		(setf (elt ranges-left axis) spec-left
		      (elt ranges-right axis) spec-right)
		(push (list onoff ranges-left) left)
		(push (list onoff ranges-right) right))))
	finally (return (list (nreverse left)
			      (nreverse right)))))

(defun faster-mwa (data)
  (multiple-value-bind (axis value) (propose-partition-axis data)
    (if axis
	(reduce '+ (partition data axis value) :key 'faster-mwa)
	(model-whole-area data))))

(defun part-2-faster-version (&optional (data (parse-input)))
  (faster-mwa data))
