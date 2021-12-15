(in-package #:advent-of-code.2021.15)

;;; Advent of code 2021: day 15
;;; see https://adventofcode.com/2021/day/15

(defun parse-input ()
  (let ((list
	  (mapcar (lambda (string)
		    (map 'list 'digit-char-p string))
		  (file-lines (my-input-path)))))
    (make-array (list (length list) (length (first list)))
		:initial-contents list)))

(defstruct (heap (:type vector))
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

(defun minimal-cost-v4 (risks)
  (destructuring-bind (h w) (array-dimensions risks)
    (let ((visited
	    (make-array (list h w) :initial-element nil))
	  (tentative-distance
	    (make-array (list h w) :initial-element nil))
	  (heap-index
	    (make-array (list h w) :initial-element nil))
	  (target
	    (complex (1- h) (1- w))))
      (flet ((betterp (c0 c1)
	       (let ((d0 (caref tentative-distance c0))
		     (d1 (caref tentative-distance c1)))
		 (or (and d0 (not d1))
		     (and d0 d1 (< d0 d1))))))
	(let ((heap (make-heap :predicate #'betterp)))
	  (setf (caref tentative-distance 0) 0
		(caref heap-index 0) 0)
	  (hpush heap 0)
	  (loop
	    (let* ((current (hpop heap)) ;; best unvisited
		   (x0 (imagpart current))
		   (y0 (realpart current))
		   (cost (caref tentative-distance current)))
	      (setf (caref visited current) t
		    (caref heap-index current) nil)
	      
	      (when (= current target)
		(return (caref tentative-distance current)))
	      
	      (loop for (dx dy) in '((0 1) (1 0) (-1 0) (0 -1))
		    for x1 = (+ x0 dx) and y1 = (+ y0 dy)
		    when (and (< -1 x1 w)
			      (< -1 y1 h))
		      do (let ((this (complex y1 x1)))
			   (unless (caref visited this)
			     (let ((old-cost (caref tentative-distance this))
				   (new-cost (+ cost (caref risks this))))
			       (when (or (not old-cost)
					 (< new-cost (caref tentative-distance this)))
				 ;; improving cost
				 (setf (caref tentative-distance this)
				       new-cost
				       (caref heap-index this)
				       (if old-cost
					   (hadjust heap (caref heap-index this))
					   (hpush heap this)))))))))))))))


(defun part-1 (&optional (data (parse-input)))
  (minimal-cost-v4 data))

(defun extend-tile (tile)
  (destructuring-bind (h w) (array-dimensions tile)
    (let ((risks (make-array (list (* h 5) (* w 5)))))
      (loop for yy below 5 do
	(loop for xx below 5 do
	  (loop for y below h do
	    (loop for x below w do
	      (setf (aref risks
			  (+ (* h yy) y)
			  (+ (* w xx) x))
		    (1+ (mod (1- (+ yy xx (aref tile y x))) 9)))))))
      risks)))

(defun part-2 (&optional (data (parse-input)))
  (minimal-cost-v4 (extend-tile data)))

