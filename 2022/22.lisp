(in-package #:advent-of-code.2022.22)

;;; Advent of code 2022: day 22
;;; see https://adventofcode.com/2022/day/22

(defun parse-route (string)
  (loop
    with cursor = 0
    while (< cursor (length string))
    collect (if (digit-char-p (char string cursor))
		(multiple-value-bind (integer pos)
		    (parse-integer string :start cursor :junk-allowed t)
		  (setf cursor pos)
		  integer)
		(prog1 (ecase (char string cursor)
			 (#\R :right)
			 (#\L :left))
		  (incf cursor)))))

(defun cnref (array complex)
  (aref array (imagpart complex)
	(realpart complex)))

(defun (setf cnref) (value array complex)
  (setf (aref array (imagpart complex)
	      (realpart complex))
	value))

(defun parse-map (lines)
  (let* ((width (reduce 'max (mapcar 'length lines)))
	 (height (length lines))
	 (array (make-array (list height width) :initial-element nil)))
    (loop for y below height
	  for line in lines
	  for skip = (position #\Space line :test-not 'eql)
	  for length = (length line) 
	  do (loop for x from skip below length
		   for right-x = (if (< (1+ x) length)
				     (1+ x)
				     skip)
		   for left-x = (if (<= skip (1- x))
				    (1- x)
				    (1- length))
		   do (setf (cnref array (complex x y))
			    (vector (complex right-x y) ;; to the right
				    nil			;; down
				    (complex left-x y)	;; to the left
				    nil			;;up
				    (ecase (char line x)
				      (#\. nil)
				      (#\# t))))))
    (loop for x below width
	  for top = (loop with y = 0
			  while (null (cnref array (complex x y)))
			  do (incf y)
			  finally (return y))
	  for bottom = (loop with y = (1- (array-dimension array 0))
			     while (null (cnref array (complex x y)))
			     do (decf y)
			     finally (return y))
	  do (loop for y from top to bottom
		   for top-y = (if (<= (1+ y) bottom)
				   (1+ y)
				   top)
		   for bot-y = (if (<= top (1- y))
				   (1- y)
				   bottom)
		   for ptrs = (cnref array (complex x y))
		   do (setf (aref ptrs 3)
			    (complex x bot-y)
			    (aref ptrs 1)
			    (complex x top-y))))
    array))

(defun parse-input ()
  (let* ((lines (file-lines (my-input-path)))
	 (map (butlast lines))
	 (route (car (last lines))))
    (list (parse-map map) (parse-route route))))

(defun starting-position (array)
  (let ((width (array-dimension array 1)))
    (loop for x below width
	  for tile = (cnref array x)
	  when (and tile (not (aref tile 4)))
	    return x)))

(defun part-1 (&optional (data (parse-input)))
  (destructuring-bind (map route) data
    (let ((location (starting-position map))
	  (facing 0))
      (dolist (item route)
	(case item
	  (:left (setf facing (mod (1- facing) 4)))
	  (:right (setf facing (mod (1+ facing) 4)))
	  (otherwise
	   (loop repeat item
		 do (let* ((next (aref (cnref map location) facing))
			   (busy (aref (cnref map next) 4)))
		      (when busy (return))
		      (setf location next))))))
      (+ (* 1000 (1+ (imagpart location)))
	 (* 4 (1+ (realpart location)))
	 facing))))

(defun dbg-print-map (array)
  (destructuring-bind (height width) (array-dimensions array)
    (loop for y below height
	  do (loop for x below width
		   for item = (cnref array (complex x y))
		   do (write-char (case item
				    ((nil) #\Space)
				    (otherwise
				     (if (aref item 4) #\# #\.)))))
	     (write-line ""))))

(defun hexamino (field)
  (destructuring-bind (height width) (array-dimensions field)
    (let ((step (if (<= height 16) 4 50)))
      (loop for y below (/ height step)
	    do (loop for x below (/ width step)
		     do (if (cnref field (complex (* x step) (* y step)))
			    (write-string "██")
			    (write-string "  ")))
	       (write-line "")))))

(defun make-proto-cube ()
  (make-array '(4 4 4) :initial-element nil))

(defparameter *facings* #(:right :down :left :up))
(defparameter +moves+ #(#c(1 0) #c(0 1) #c(-1 0) #c(0 -1)))


(defun rotate-coord (face x y z)
  "
DEF. Front facet has z=0, inner square coords
(1,1) (2,1)
(1,2) (2,2)

Translate coordinates in source cube to bring the facet at FACE
direction from the front facet to the front."
  (ecase face
    (:up    (values x (- 3 z) y))
    (:down  (values x z (- 3 y)))
    (:left  (values (- 3 z) y x))
    (:right (values z y (- 3 x)))))

(defun rotate-cube (face cube)
  (let ((cube-prim (make-proto-cube)))
    (dotimes (x 4 cube-prim)
      (dotimes (y 4)
	(dotimes (z 4)
	  (let ((output (multiple-value-list (rotate-coord face x y z))))
	    (apply #'(setf aref) (aref cube x y z) cube-prim output)))))))

(defun testy-cube ()
  (let ((cube (make-proto-cube)))
    (dotimes (x 4 cube)
      (dotimes (y 4)
	(dotimes (z 4)
	  (setf (aref cube x y z) (list x y z)))))))

(defun print-cube-front (cube)
  (format t "~&~5a~5a~&~5a~5a"
	  (aref cube 1 1 0)
	  (aref cube 2 1 0)
	  (aref cube 1 2 0)
	  (aref cube 2 2 0)))

(defun map-hexamino (field)
  (let ((redfield (make-array '(4 4) :element-type 'bit :initial-element 0)))
   (destructuring-bind (height width) (array-dimensions field)
     (let ((step (if (<= height 16) 4 50)))
       (loop for y below (/ height step)
	     do (loop for x below (/ width step)
		      do (when (cnref field (complex (* x step) (* y step)))
			   (setf (cnref redfield (complex x y)) 1))))
       (values redfield step)))))

(defun mod4-neighbors (complex)
  (flet ((valid (c)
	   (and (<= 0 (realpart c) 3)
		(<= 0 (imagpart c) 3))))
    (loop for delta across +moves+
	  for facing across *facings*
	  when (valid (+ complex delta))
	    collect (cons facing (+ complex delta)))))

(defun hexamino-path-map (harray)
  (let ((start (loop for x below 4
		     when (plusp (cnref harray x))
		       do (return x)))
	(pathmap (make-array '(4 4) :initial-element t)))
    (assert (= 1 (cnref harray start)))
    (setf (cnref pathmap start) nil)
    (let ((front (list start))
	  (visited (make-hash-table))
	  (next nil))
      (loop while front
	    do (loop for cell in front
		     do (setf (gethash cell visited) t)
			(loop for (step . to-cell) in (mod4-neighbors cell)
			      when (and (= 1 (cnref harray to-cell))
					(not (gethash to-cell visited)))
				do (push to-cell next)
				   (setf (cnref pathmap to-cell)
					 (cons step (cnref pathmap cell)))))
	       (shiftf front next nil)))
    pathmap))

(defun invert-step (symbol)
  (ecase symbol
    (:left :right)
    (:right :left)
    (:up :down)
    (:down :up)))

(defun stamp-cube (dim path-map)
  (let ((cube (make-proto-cube)))
    (dotimes (x 4 cube)
      (dotimes (y 4)
	(let ((path (cnref path-map (complex x y))))
	  (unless (eq t path)
	    (loop for movement in (reverse path)
		  do (setf cube (rotate-cube movement cube)))

	    ;; front cube face corresponds to map area (x,y)*dim
	    (assert (null (aref cube 1 1 0)))
	    (setf (aref cube 1 1 0) (complex (* x dim)              (* y dim))
		  (aref cube 2 1 0) (complex (+ (* x dim) (1- dim)) (* y dim))
		  (aref cube 1 2 0) (complex (* x dim) (+ (* y dim) (1- dim)))
		  (aref cube 2 2 0) (complex (+ (* x dim) (1- dim))
					     (+ (* y dim) (1- dim))))
	    (loop for movement in path
		  do (setf cube (rotate-cube (invert-step movement) cube)))))))))

(defun ccsignum (complex)
  (complex (signum (realpart complex))
	   (signum (imagpart complex))))

(defun where-to-glue (cube)
  (let ((glued nil))
    ;; rotate right four times, in each position do:
    (dotimes (i 4 glued)
      ;; (mutually) glue the front facet with the facet to the right
      (push (list (aref cube 2 1 0) (aref cube 2 2 0)
		  (aref cube 3 1 1) (aref cube 3 2 1)
		  (position (ccsignum (- (aref cube 2 1 0)
					 (aref cube 1 1 0)))
			    +moves+)
		  (position (ccsignum (- (aref cube 3 1 2)
					 (aref cube 3 1 1)))
			    +moves+))
	    glued)
      ;; glue the front facet to the top facet
      (push (list (aref cube 1 1 0) (aref cube 2 1 0)
		  (aref cube 1 0 1) (aref cube 2 0 1)
		  (position (ccsignum (- (aref cube 1 1 0)
					 (aref cube 1 2 0)))
			    +moves+)
		  (position (ccsignum (- (aref cube 1 0 2)
					 (aref cube 1 0 1)))
			    +moves+))
	    glued)
      ;; glue the front facet to the bottom facet
      (push (list (aref cube 1 2 0) (aref cube 2 2 0)
		  (aref cube 1 3 1) (aref cube 2 3 1)
		  (position (ccsignum (- (aref cube 1 2 0)
					 (aref cube 1 1 0)))
			    +moves+)
		  (position (ccsignum (- (aref cube 1 3 2)
					 (aref cube 1 3 1)))
			    +moves+))
	    glued)
      
      (setf cube (rotate-cube :right cube)))))

;;; cheating (hardcoded glue data for my input) 
(defun part-2-dirty (&optional (data (parse-input)))
  (destructuring-bind (map route) data
    (destructuring-bind (height width) (array-dimensions map)
      (dotimes (y height)
	(dotimes (x width)
	  (when-let ((dir (cnref map (complex x y))))
	    (dotimes (facing 4)
	      (setf (aref dir facing)
		    (cons facing (aref dir facing))))))))
    (flet ((glue (cstart1 cend1 cstart2 cend2 f1 f2)
	     (print (list cstart1 cend1 cstart2 cend2 f1 f2))
	     (let ((d1 (complex (signum (- (realpart cend1) (realpart cstart1)))
				(signum (- (imagpart cend1) (imagpart cstart1)))))
		   (d2 (complex (signum (- (realpart cend2) (realpart cstart2)))
				(signum (- (imagpart cend2) (imagpart cstart2))))))
	       (loop with p1 = cstart1
		     with p2 = cstart2
		     for almost-done = (= p1 cend1)
		     do (setf (aref (cnref map p1) f1)
				 (cons f2 p2))
			(setf (aref (cnref map p2) (mod (+ 2 f2) 4))
			      (cons (mod (+ 2 f1) 4) p1))
			(incf p1 d1)
			(incf p2 d2)
			(when almost-done
			  (return))))))

      (glue #c(99 50) #c(99 99) #c(100 49) #c(149 49) 0 3)
      (glue #c(99 100) #c(99 149) #c(149 49) #c(149 0) 0 2)
      (glue #c(50 50) #c(50 99) #c(0 100) #c(49 100) 2 1)
      (glue #c(50 0) #c(50 49) #c(0 149) #c(0 100) 2 0)
      (glue #c(50 0) #c(99 0) #c(0 150) #c(0 199) 3 0)
      (glue #c(100 0) #c(149 0) #c(0 199) #c(49 199) 3 3)
      (glue #c(50 149) #c(99 149) #c(49 150) #c(49 199) 1 2))

    (let ((location (starting-position map))
	  (facing 0))
      (dolist (item route)
	(case item
	  (:left (setf facing (mod (1- facing) 4)))
	  (:right (setf facing (mod (1+ facing) 4)))
	  (otherwise
	   (loop repeat item
		 do 
		    (let* ((next (aref (cnref map location) facing))
			   (busy (aref (cnref map (cdr next)) 4)))
		      (when busy (return))
		      (setf location (cdr next)
			    facing (car next)))))))
      (+ (* 1000 (1+ (imagpart location)))
	 (* 4 (1+ (realpart location)))
	 facing))))

(defun part-2-real (&optional (data (parse-input)))
  (destructuring-bind (map route) data
    (destructuring-bind (height width) (array-dimensions map)
      (dotimes (y height)
	(dotimes (x width)
	  (when-let ((dir (cnref map (complex x y))))
	    (dotimes (facing 4)
	      (setf (aref dir facing)
		    (cons facing (aref dir facing))))))))
    
    (multiple-value-bind (hexamino unit) (map-hexamino map)
      (flet ((glue (cstart1 cend1 cstart2 cend2 f1 f2)
	       (let ((d1 (ccsignum (- cend1 cstart1)))
		     (d2 (ccsignum (- cend2 cstart2))))
		 (loop with p1 = cstart1
		       with p2 = cstart2
		       for almost-done = (= p1 cend1)
		       do (setf (aref (cnref map p1) f1)
				   (cons f2 p2))
			  (setf (aref (cnref map p2) (mod (+ 2 f2) 4))
				(cons (mod (+ 2 f1) 4) p1))
			  (incf p1 d1)
			  (incf p2 d2)
			  (when almost-done (return))))))
	
	(loop for glue-args in (where-to-glue (stamp-cube unit (hexamino-path-map hexamino)))
	      do (apply #'glue glue-args))))

    (let ((location (starting-position map))
	  (facing 0))
      (dolist (item route)
	(case item
	  (:left (setf facing (mod (1- facing) 4)))
	  (:right (setf facing (mod (1+ facing) 4)))
	  (otherwise
	   (loop repeat item
		 do 
		    (let* ((next (aref (cnref map location) facing))
			   (busy (aref (cnref map (cdr next)) 4)))
		      (when busy (return))
		      (setf location (cdr next)
			    facing (car next)))))))
      (+ (* 1000 (1+ (imagpart location)))
	 (* 4 (1+ (realpart location)))
	 facing))))
