(in-package #:advent-of-code.2020.20)

;;; Advent of code 2020: day 20
;;; see https://adventofcode.com/2020/day/20

(defun parse-input ()
  (mapcar #'parse-tile (line-groups (file-lines (my-input-path) :remove-empty nil))))

(defun parse-tile (tile)
  (cons
   (parse-integer (first tile) :start 5 :end 9)
   (let ((bits (make-array '(10 10) :element-type 'bit)))
     (prog1 bits
       (loop for line in (rest tile)
	     for y from 0 below 10
	     do (loop for char across line
		      for x from 0 below 10
		      do (setf (aref bits x y)
			       (ecase char (#\# 1) (#\. 0)))))))))

(defun cref (array complex)
  (let ((x (realpart complex))
	(y (imagpart complex)))
    (destructuring-bind (width height)
	(array-dimensions array)
      (and (< -1 x width)
	   (< -1 y height)
	   (aref array (realpart complex) (imagpart complex))))))

(defun (setf cref) (new-value array complex)
  (setf (aref array (realpart complex) (imagpart complex)) new-value))

(defun part-1 (&optional (data (parse-input)))
  (labels ((edge (bits start step)
	     (loop for bit from 0 below 10
		   for index = start then (+ index step)
		   sum (ash (cref bits index) bit)))

	   (flip (d)
	     (loop for bit from 0 below 10
		   sum (ash (if (logbitp (- 9 bit) d) 1 0) bit)))

	   (edges (bits)
	     (loop for (start step) in
		   '((#C(0 0) #C(0 1 ))
		     (#C(0 9) #C(1 0 ))
		     (#C(9 9) #C(0 -1))
		     (#C(9 0) #C(-1 0)))
		   collect (edge bits start step))))

    (let ((tiles data)
	  (hash-table (make-hash-table)))

      (loop
	for (id . bits) in tiles
	do (dolist (edge (edges bits))
	     (pushnew id (gethash edge hash-table))))

      (flet ((unknown (edge by)
	       (endp (remove by (gethash edge hash-table)))))

	(let ((corners
		(loop
		  for (id . bits) in tiles
		  when (>= (loop for edge in (edges bits)
				 count (and (unknown edge id)
					    (unknown (flip edge) id)))
			   2)
		    collect id)))
	  (assert (= 4 (length corners)))
	  (apply #'* corners))))))


(defun part-2 (&optional (data (parse-input)))
  (labels ((edge (bits start step)
	     (loop for bit from 0 below 10
		   for index = start then (+ index step)
		   sum (ash (cref bits index) bit)))

	   (flip (d)
	     (loop for bit from 0 below 10
		   sum (ash (if (logbitp (- 9 bit) d) 1 0) bit)))

	   (edges (bits)
	     (loop for (start step) in
		   '((#C(0 0) #C(1 0 ))
		     (#C(9 0) #C(0 1 ))
		     (#C(9 9) #C(-1 0))
		     (#C(0 9) #C(0 -1)))
		   collect (edge bits start step)))

	   ;; transpose
	   (flip-bits (bits)
	     (let* ((dim (array-dimension bits 0))
		    (result (make-array (list dim dim) :element-type 'bit)))
	       (dotimes (y dim result)
		 (dotimes (x dim)
		   (setf (aref result y x)
			 (aref bits x y))))))

	   (rotate-bits (bits) ;; pi/2 counterclockwise
	     (let* ((dim (array-dimension bits 0))
		    (result (make-array (list dim dim) :element-type 'bit)))
	       (dotimes (y dim result)
		 (dotimes (x dim)
		   (setf (aref result x y) ;; 0,0 <- 9,0 ; 1,0 <- 9,1 ; 0,1 <- 8,0
			 (aref bits (- (1- dim) y) x)))))))

    (let* ((tiles data)
	   (hash-table (make-hash-table))
	   (tile-count (length tiles))
	   (image-size (isqrt tile-count))
	   (image (make-array (list image-size image-size)
			      :initial-element nil))
	   (full-image-size (* image-size 8))
	   (output-bits (make-array (list full-image-size full-image-size) :element-type 'bit))
	   (used))

      (assert (= tile-count (expt image-size 2)))

      (loop
	for (id . bits) in tiles
	do (dolist (edge (edges bits))
	     (pushnew id (gethash edge hash-table))))

      (labels ((edge-match (pattern edge)
		 (or (null pattern)
		     (and (eq pattern t)
			  (= (+ (length (gethash edge hash-table))
				(length (gethash (flip edge) hash-table))) 1))
		     (eql pattern edge)))

	       (find-tile (pattern candidates)
		 (let ((found))
		   (loop for tile in (remove-duplicates candidates)
			 for bits = (cdr (assoc tile tiles))
			 do (loop
			      for flipped = bits then (flip-bits flipped)
			      repeat 2
			      do (loop for rotated = flipped then (rotate-bits rotated)
				       repeat 4
				       when (every #'edge-match pattern
						   (edges rotated))
					 do (when found
					      (if (= tile (car found))
						  (format t "~&pseudo-dup ~a ~a~&" tile (car found))
						  (format t "~&pseudo-ns-dup ~a ~a~&" tile (car found))))
					    (when (and found (not (equalp (cdr found) rotated)))
					      (return-from find-tile (values nil t)))
					    (setf found (cons tile rotated)))))
		   found)))
	;;
	(let ((corners (flet ((unknown (edge by)
				(or (not (gethash edge hash-table))
				    (equal (list by) (gethash edge hash-table)))))

			 (loop
			   for (id . bits) in tiles
			   for outer-edges = (loop for edge in (edges bits)
						   when (and (unknown edge id)
							     (unknown (flip edge) id))
						     collect edge)
			   when (>= (length outer-edges) 2)
			     collect (list id outer-edges)))))
	  (setf (aref image 0 0) (cdr (assoc (car (first corners)) tiles)))
	  ;;
	  (loop for edges = (edges (aref image 0 0))
		until (every #'edge-match '(t nil nil t) edges)
		do (setf (aref image 0 0)
			 (rotate-bits (aref image 0 0))))

	  (push (car (first corners)) used)

	  (loop with unknown-cells = (1- (* image-size image-size))
		until (zerop unknown-cells)
		do (loop for y below image-size
			 do (loop for x below image-size
				  for point = (complex x y)
				  when (null (cref image point))
				    do (let ((neighbours
					       (loop for off in '(#C(0 -1) ;; top right bottom left
								  #C(1 0)
								  #C(0 1)
								  #C(-1 0))

						     and (start step) in '((#C(0 9) #C(1 0)) ;; top
									   (#C(0 0) #C(0 1)) ;; right
									   (#C(9 0) #C(-1 0)) ;; bottom
									   (#C(9 9) #C(0 -1 )))

 						     for neighbour = (cref image (+ point off))
						     collect (and neighbour
								  (edge neighbour start step)
								  ))))
					 (unless (every 'null neighbours)
					   (let ((interesting-tiles
						   (loop for edge in neighbours
							 when edge
							   append (gethash edge hash-table)
							   and append (gethash (flip edge) hash-table))))

					     (setf interesting-tiles
						   (set-difference interesting-tiles used))

					     (assert interesting-tiles)
					     (multiple-value-bind
						   (found many) (find-tile neighbours interesting-tiles)
					       (when found
						 (push (car found) used)
						 (decf unknown-cells)
						 (setf (aref image x y)
						       (cdr found))))))))))
	  (dotimes (x full-image-size)
	    (dotimes (y full-image-size)
	      (setf (aref output-bits x y)
		    (aref
		     (aref image (floor x 8) (floor y 8))
		     (1+ (mod x 8)) (1+ (mod y 8))))))
	  (let* ((monster
		   "
                  #
#    ##    ##    ###
 #  #  #  #  #  #   ")
		 (monster (subseq monster 1))
		 (monster (split-sequence:split-sequence #\Newline monster))
		 (monster-width (length (first monster)))
		 (monster-height (length monster))
		 (monster-bits (make-array (list monster-width monster-height)
					   :element-type 'bit)))
	    (loop for y from 0 to monster-height
		  for line in monster
		  do (loop for x from 0 to monster-width
			   for char across line
			   do (setf (aref monster-bits x y)
				    (ecase char (#\# 1) (#\Space 0)))))

	    (loop for flipped = output-bits then (flip-bits flipped)
		  for flipness below 2
		  nconc
		  (loop for rotated = flipped then (rotate-bits rotated)
			for angle below 4
			nconc (loop for x-off from 0 below (- full-image-size monster-width)
				    nconc (loop for y-off from 0 below (- full-image-size monster-height)
						when
						(loop for x from 0 below monster-width
						      always
						      (loop for y from 0 below monster-height
							    always (or (zerop
									(aref monster-bits x y))
								       (= 1
									  (aref rotated
										(+ x x-off)
										(+ y y-off))))))
						collect (cons rotated (complex x-off y-off)))))
		    into monsters
		  finally
		     (return
		       (progn
			 (setf output-bits (car (first monsters)))
			 (dolist (offset (mapcar #'cdr monsters))
			   (dotimes (y monster-height)
			     (dotimes (x monster-width)
			       (when (= 1 (aref monster-bits x y))
				 (setf (aref output-bits
					     (+ x (realpart offset))
					     (+ y (imagpart offset)))
				       0)))))
			 (loop for y below full-image-size
			       sum (loop for x below full-image-size
					 count (= 1 (aref output-bits x y)))))))))))))
