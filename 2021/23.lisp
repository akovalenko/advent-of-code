(in-package #:advent-of-code.2021.23)

;;; Advent of code 2021: day 23
;;; see https://adventofcode.com/2021/day/23

(defstruct heap
  (data (make-sequence 'simple-vector 10 :initial-element nil)
   :type simple-vector)
  (predicate #'<
   :type function)
  (key #'identity
   :type function)
  (length 0
   :type array-index))

(defun sift-up (heap index)
  (let ((vector (heap-data heap))
	(betterp (heap-predicate heap))
	(key-fn (heap-key heap)))
    (declare (array-index index))
    (loop
      (when (zerop index)
	(return index))
      (let ((parent (ash (1- index) -1)))
	(unless
	    (funcall betterp
		     (funcall key-fn (svref vector index))
		     (funcall key-fn (svref vector parent)))
	  (return index))
	(rotatef (svref vector index)
		 (svref vector parent))
	(setf index parent)))))

(defun sift-down (heap index)
  (declare (array-index index))
  (let ((vector (heap-data heap))
	(betterp (heap-predicate heap))
	(key-fn (heap-key heap))
	(length (heap-length heap)))
    (loop
      (let* ((d (ash index 1))
	     (left (+ 1 d))
	     (right (+ 2 d)))
	(unless (< left length)
	  (return index))
	(let ((rootest-child
		(if (or (not (< right length))
			(funcall betterp
				 (funcall key-fn (svref vector left))
				 (funcall key-fn (svref vector right))))
		    left right)))
	  (when (funcall betterp
			 (funcall key-fn (svref vector index))
			 (funcall key-fn (svref vector rootest-child)))
	    (return index))
	  
	  (rotatef (svref vector index)
		   (svref vector rootest-child))
	  (setf index rootest-child))))))

(defun hpush (heap value)
  (let ((vector (heap-data heap))
	(length (heap-length heap)))
    (if (= (length vector) length)
	(let ((new (make-sequence 'simple-vector (floor (* (length vector) 3/2))
				  :initial-element nil)))
	  (replace new vector)
	  (setf (svref new length) value
		(heap-data heap) new
		(heap-length heap) (1+ length)))
	(setf (svref vector length) value
	      (heap-length heap) (1+ length)))
    (sift-up heap length)))

(defun hpop (heap)
  (prog1 (svref (heap-data heap) 0)
    (setf (svref (heap-data heap) 0)
	  (svref (heap-data heap) (1- (heap-length heap)))
	  (heap-length heap)
	  (1- (heap-length heap)))
    (sift-down heap 0)))

(defun hadjust (heap pos)
  (let ((up (sift-up heap pos)))
    (if (= up pos)
	(sift-down heap pos)
	up)))


;; amphipod location encoding:
;; there are 11 + 8 cells, 0-10 being the top row,
;; 11-12 the A room, ... 17-18 the D room.
;;
;; First A can be located in 19 cells, next A in 18 cells,
;; plus we enforce first A < next A, giving 19*18/2 = 171 A locations.
;;
;; After that, first B can be located in 17 cells, next B in 16 cells,
;; modulo ordering we have 17*16/2 = 136 B locations
;;
;; C => 15*14/2 = 105 locations
;; D => 13*12/2 = 78 locations
;;
;; Total variants = (* 171 136 105 78) = 190466640
;;
;; Scratch that, there are 4 forbidden stationary points (room exits),
;; so we have
;;
;; As => 15*14/2 = 105 locations
;; Bs => 13*12/2 = 78 locations
;; Cs => 11*10/2 = 55 locations
;; Ds => 9*8/2 = 36 locations
;;
;; Total variants = (* 105 78 55 36) = 16216200

;; Coordinates:
;; #############
;; #89 A B C DE#
;; ###1#3#5#7###
;;   #0#2#4#6#
;;   #########

;; encoding:
;; Cmin, Cmax, Power => think out later
;; 
;; to hell with minimalistic encoding, let's just do AABBCCDD with ordering enforcement
;; and (mod 16) position to see 


(defparameter *map*
  '("#############"
    "#89 A B C DE#"
    "###1#3#5#7###"
    "  #0#2#4#6#"))

(defparameter *holy-grail*
  '("#############"
    "#           #"
    "###A#B#C#D###"
    "  #A#B#C#D#"))

(defun nth-pod (n)
  (char "ABCD" n))

(defun pod-p (char)
  (let ((digit-char-p (digit-char-p char 16)))
    (and digit-char-p
	 (<= #xA digit-char-p #xD)
	 (- digit-char-p #xA))))

(defun id-for-lines (lines)
  (loop	for line in lines
	for pattern in *map*
	sum (loop
	      for char across line
	      for cell-id across pattern
	      for pod-p = (pod-p char)
	      when pod-p
		sum (let ((pos (digit-char-p cell-id 16)))
		      (dpb (1+ pod-p) (byte 3 (* 3 pos)) 0)))))

;; construct table: (pos1 , pos2) => cost, intermediates to check incl pos2

(defun map-topology (map)
  (let ((h (length map))
	(result (make-array '(15 15) :initial-element nil))
	(neighbours '((0 1) (0 -1) (1 0) (-1 0))))
    (labels ((at (y x) (or (and (< -1 y h)
				(let ((line (elt map y)))
				  (and (< -1 x (length line))
				       (char (elt map y) x))))
			   #\#))
	     (wallp (y x) (char= #\# (at y x)))
	     (coord (y x) (digit-char-p (at y x) 16))
	     (mask-of (coord) (if coord (ash #b111 (* 3 coord)) 0))
	     (explore (y0 x0 coord)
	       (loop with visited = (list (list y0 x0))
		     with front = (list (list y0 x0 0 0))
		     do (setf
			 front
			 (loop for (ya xa cost mask) in front
			       nconc (loop for (dy dx) in neighbours
					   for yb = (+ ya dy)
					   and xb = (+ xa dx)
					   for cb = (coord yb xb)
					   for new-mask
					     = (logior mask (mask-of cb))
					   for new-cost = (1+ cost)
					   when (and
						 (not (wallp yb xb))
						 (not
						  (eq visited
						      (pushnew
						       (list yb xb)
						       visited
						       :test 'equal))))
					     collect (list yb xb new-cost new-mask)
					     and do (when cb
						      (setf
						       (aref result coord cb)
						       (list new-cost new-mask))))))
			(unless front (return)))))
      (loop for y0 below h and line in map
	    do (loop for x0 from 0 and char across line
		     for coord = (coord y0 x0)
		     when coord do (explore y0 x0 coord))
	    finally (return result)))))

(defun generate-motions (id)
  (loop
    with topology = (load-time-value (map-topology *map*))
    for coord below 15
    for occupier = (ldb (byte 3 (* coord 3)) id)
    unless (zerop occupier)
      nconc (loop for new below 15
		  for path = (aref topology coord new)
		  when (and (not (= new coord))
			    path
			    (zerop (logand (second path) id))
			    ;; specific rules:
			    (not
			     (and (<= 8 coord) ;; from the hallway
				  (<= new 7)   ;; into a room
				  (or (/= (1- occupier) (floor new 2)) ;; non-owned
				      (let* ((nrb (* 2 (1- occupier)))
					     (oth (+ nrb (if (= nrb new) 1 0)))
					     (nei (ldb (byte 3 (* 3 oth)) id)))
					(and (not (zerop nei))
					     (/= occupier nei))))))
			    (not
			     (and (<= 8 coord) ;; from the hallway
				  (<= 8 new))) ;; into the hallway
			    (not ;; within the same room, up
			     (and (< coord 8)
				  (< new 8)
				  (= (floor coord 2)
				     (floor new 2))
				  (oddp new)))
			    (not ;; with
			     (and (< new 8)
				  (< coord 8)))
			    (not
			     (and (< new 8)
				  (oddp new)
				  (zerop (ldb (byte 3 (* 3 (1- new))) id)))))
		    
		    collect
		    (let ((new-id id))
		      (setf (ldb (byte 3 (* coord 3)) new-id) 0
			    (ldb (byte 3 (* new 3)) new-id) occupier)
		      (cons new-id
			    (* (first path)
			       (svref #(1 10 100 1000) (1- occupier))))))))

(defun reach-holy-grail (id)
  (let ((queue (make-heap :key #'cdr))
	(target (id-for-lines *holy-grail*)))
    (hpush queue (cons id 0))
    (loop
      with visited = (make-hash-table)
      and old-min-cost = 0
      for (this . cost) = (hpop queue)
      when (= target this)
	return cost
      unless (= old-min-cost cost)
	do (setf old-min-cost cost)
	   (print cost)
      unless (shiftf (gethash this visited) t)
	do (loop for (other . delta) in (generate-motions this)
		 for new-cost = (+ cost delta)
		 do (unless (gethash other visited)
		      (hpush queue (cons other new-cost)))))))


(defun parse-input ()
  (id-for-lines (file-lines (my-input-path))))

(defun part-1 (&optional (data (parse-input)))
  (reach-holy-grail data))

(defparameter *map2*
  '("#############"
    "#89 A B C DE#"
    "###1#3#5#7###"
    "  #0#2#4#6#"
    "  #F#H#J#L#"
    "  #G#I#K#M#"))

(defparameter *holy-grail-2*
  '("#############"
    "#           #"
    "###A#B#C#D###"
    "  #A#B#C#D#"
    "  #A#B#C#D#"
    "  #A#B#C#D#"))

(defparameter *paste-2*
  '("  #D#C#B#A#"
    "  #D#B#A#C#"))

(defun map-topology-2 (map)
  (let ((h (length map))
	(result (make-array '(23 23) :initial-element nil))
	(neighbours '((0 1) (0 -1) (1 0) (-1 0))))
    (labels ((at (y x) (or (and (< -1 y h)
				(let ((line (elt map y)))
				  (and (< -1 x (length line))
				       (char (elt map y) x))))
			   #\#))
	     (wallp (y x) (char= #\# (at y x)))
	     (coord (y x) (digit-char-p (at y x) 24))
	     (mask-of (coord) (if coord (ash #b111 (* 3 coord)) 0))
	     (explore (y0 x0 coord)
	       (loop with visited = (list (list y0 x0))
		     with front = (list (list y0 x0 0 0))
		     do (setf
			 front
			 (loop for (ya xa cost mask) in front
			       nconc (loop for (dy dx) in neighbours
					   for yb = (+ ya dy)
					   and xb = (+ xa dx)
					   for cb = (coord yb xb)
					   for new-mask
					     = (logior mask (mask-of cb))
					   for new-cost = (1+ cost)
					   when (and
						 (not (wallp yb xb))
						 (not
						  (eq visited
						      (pushnew
						       (list yb xb)
						       visited
						       :test 'equal))))
					     collect (list yb xb new-cost new-mask)
					     and do (when cb
						      (setf
						       (aref result coord cb)
						       (list new-cost new-mask))))))
			(unless front (return)))))
      (loop for y0 below h and line in map
	    do (loop for x0 from 0 and char across line
		     for coord = (coord y0 x0)
		     when coord do (explore y0 x0 coord))
	    finally (return result)))))

(defun id-for-lines-2 (lines)
  (loop	for line in lines
	for pattern in *map2*
	sum (loop
	      for char across line
	      for cell-id across pattern
	      for pod-p = (pod-p char)
	      when pod-p
		sum (let ((pos (digit-char-p cell-id 24)))
		      (dpb (1+ pod-p) (byte 3 (* 3 pos)) 0)))))

(defun parse-input-2 ()
  (let ((lines (file-lines (my-input-path))))
    (id-for-lines-2 (concatenate 'list (subseq lines 0 3)
				 *paste-2* (subseq lines 3)))))

(defun good-and-progressive-move-v2 (occupier id coord new)
  (flet ((at (x) (ldb (byte 3 (* x 3)) id))
	 (roomp (x)
	   (cond ((< x 8) (floor x 2))
		 ((< 14 x) (floor (- x 15) 2))))
	 (height (x)
	   (cond ((< x 8)  (+ 2 (mod x 2)))
		 ((< 14 x) (mod x 2))))
	 (room-place (n height)
	   (if (< height 2)
	       (+ 15 (* 2 n) (- 1 height))
	       (+ (* 2 n) (- height 2)))))
    (let* ((to-room (roomp new))
	   (from-room (roomp coord))
	   (native-place (1- occupier))
	   (from-hallway (not from-room))
	   (to-hallway (not to-room)))
      
      (or (and from-room to-room
	       (not (= from-room to-room))
	       (= to-room native-place)
	       (loop for under below (height new)
		     always (= occupier (at (room-place new under)))))
	  
	  (and from-room
	       to-hallway
	       (not (and (= from-room native-place)
			 (loop for under below (height coord)
			       always (= occupier (at (room-place from-room under)))))))
	  (and from-hallway to-room
	       (= native-place to-room)
	       (loop for under below (height new)
		     always (= occupier (at (room-place to-room under)))))))))

(defun generate-motions-2 (id)
  (loop
    with topology = (load-time-value (map-topology-2 *map2*))
    for coord below 23
    for occupier = (ldb (byte 3 (* coord 3)) id)
    unless (zerop occupier)
      nconc (loop for new below 23
		  for path = (aref topology coord new)
		  when (and (not (= new coord))
			    path
			    (zerop (logand (second path) id))
			    ;; specific rules:
			    (good-and-progressive-move-v2 occupier id coord new))
		    
		    collect
		    (let ((new-id id))
		      (setf (ldb (byte 3 (* coord 3)) new-id) 0
			    (ldb (byte 3 (* new 3)) new-id) occupier)
		      (cons new-id
			    (* (first path)
			       (svref #(1 10 100 1000) (1- occupier))))))))

(defun reach-holy-grail-2 (id)
  (let ((queue (make-heap :key #'cdr))
	(target (id-for-lines-2 *holy-grail-2*)))
    (hpush queue (cons id 0))
    (loop
      with visited = (make-hash-table)
      and old-min-cost = 0
      for (this . cost) = (hpop queue)
      when (= target this)
	return cost
      unless (= old-min-cost cost)
	do (setf old-min-cost cost)
	   (print cost)
      unless (shiftf (gethash this visited) t)
	do (loop for (other . delta) in (generate-motions-2 this)
		 for new-cost = (+ cost delta)
		 when (< cost 50000)
		   do (unless (gethash other visited)
			(hpush queue (cons other new-cost)))))))

(defun print-id2 (id)
  (loop for line in *map2*
	do (loop for char across line
		 do (case char
		      ((#\# #\Space) (write-char char))
		      (otherwise
		       (let ((me (ldb (byte 3 (* 3 (digit-char-p char 24))) id)))
			 (write-char (if (= me 0) #\Space
					 (digit-char (+ 9 me) 16)))))))
	   (write-line "")))

(defun part-2 (&optional (data (parse-input-2)))
  (reach-holy-grail-2 data))
