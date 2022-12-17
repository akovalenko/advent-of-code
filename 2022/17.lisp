(in-package #:advent-of-code.2022.17)

;;; Advent of code 2022: day 17
;;; see https://adventofcode.com/2022/day/17

(defun parse-input ()
  (first (file-lines (my-input-path))))


(defparameter *rock-bits*
  '#(#(15)
     #(2 7 2)
     #(4 4 7)
     #(1 1 1 1)
     #(3 3)))


(defun part-1 (&optional (data (parse-input)) (blks 2022))
  (let ((well (list 127))
	(block-type 0)
	(wind-step 0))
    (flet ((get-block ()
	     (prog1 (aref *rock-bits* block-type)
	       (setf block-type (mod (1+ block-type) (length *rock-bits*)))))
	   (get-wind ()
	     (prog1 (aref data wind-step)
	       (setf wind-step (mod (1+ wind-step) (length data)))))
	   (clashp (blk x y)
	     (cond ((< x 0) t) ;; left edge
		   ((loop for line across blk
			    thereis (logbitp 7 (ash line x))) ;; right edge
		    t)
		   ((loop
		      with zone = (- y)
		      for busy in well
		      for i below zone
		      for bix from (- (length blk) zone)
			thereis (and (< -1 bix (length blk))
				     (not (zerop (logand busy (ash (aref blk bix) x))))))
		    t)))

	   (establish (blk x y)
	     (assert (<= y 0)) ;; can't move down
	     (loop for i below (+ y (length blk))
		   do (push 0 well))
	     ;;(print well)
	     (decf y (max 0 (+ y (length blk))))
	     (loop for line across blk
		   for head on (nthcdr (- (+ y (length blk))) well)
		   do (setf (car head)
			    (logxor (car head)
				    (ash line x))))))
      
      (loop
	repeat blks ;; let figure appear
	do (let* ((blk (get-block))
		  (x 2) ;; from the left
		  (y 3)) ;; between the rock and the well top, may be negative
	     (loop
	       (let ((dx (ecase (get-wind) (#\< -1) (#\> 1))))
		 (unless (clashp blk (+ x dx) y)
		   (incf x dx)))
	       (if (clashp blk x (1- y))
		   (progn (establish blk x y)
			  (return))
		   (decf y)))))
      (1- (length well)))))

;;; this solution doesn't work for test data in my input, only with
;;; real data: it's based on an observation that anything under the
;;; full line doesn't influence further process, and my test data
;;; produce no full lines ever.

(defun part-2 (&optional (data (parse-input)) (blks 1000000000000))
  (let ((well (list 127))
	(block-type 0)
	(wind-step 0))
    (flet ((get-block ()
	     (prog1 (aref *rock-bits* block-type)
	       (setf block-type (mod (1+ block-type) (length *rock-bits*)))))
	   (get-wind ()
	     (prog1 (aref data wind-step)
	       (setf wind-step (mod (1+ wind-step) (length data)))))
	   (clashp (blk x y)
	     (cond ((< x 0) t) ;; left edge
		   ((loop for line across blk
			    thereis (logbitp 7 (ash line x))) ;; right edge
		    t)
		   ((loop
		      with zone = (- y)
		      for busy in well
		      for i below zone
		      for bix from (- (length blk) zone)
			thereis
			(and (< -1 bix (length blk))
			     (not (zerop (logand busy (ash (aref blk bix) x))))))
		    t)))

	   (establish (blk x y)
	     (assert (<= y 0)) ;; can't move down
	     (loop for i below (+ y (length blk))
		   do (push 0 well))
	     ;;(print well)
	     (decf y (max 0 (+ y (length blk))))
	     (loop for line across blk
		   for head on (nthcdr (- (+ y (length blk))) well)
		   do (setf (car head)
			    (logxor (car head)
				    (ash line x))))))
      
      (loop
	with seen = (make-hash-table :test 'equal)
	for move from 0
	do (let* ((id (list* block-type wind-step
			     (subseq well 0 (position 127 well))))
		  (saw (gethash id seen)))
	     (when saw
	       (let* ((period-moves (- move saw))
		      (height-then (part-1 data saw))
		      (height (1- (length well)))
		      (period-height (- height height-then)))
		 (multiple-value-bind (div mod) (floor (- blks saw) period-moves)
		   (return (+ (part-1 data (+ mod saw))
			      (* div period-height))))))
	     (setf (gethash id seen) move))
	do (let* ((blk (get-block)) ;; let figure appear
		  (x 2)		    ;; from the left
		  (y 3)) ;; between the rock and the well top, may be negative
	     (loop
	       (let ((dx (ecase (get-wind) (#\< -1) (#\> 1))))
		 (unless (clashp blk (+ x dx) y)
		   (incf x dx)))
	       (if (clashp blk x (1- y))
		   (progn (establish blk x y)
			  (return))
		   (decf y))))))))

;;; this solution is somewhat probabilistic, assuming that in the same
;;; phase and the same 100 top lines we get the same result. It can be
;;; proven for particular data set by tracking final depths for each
;;; settled block, but the proof is not attempted here. However, this
;;; thing works for both the test data and the real input.

(defun experiment-2 (&optional (data (parse-input)) (blks 1000000000000))
  (let ((well (list 127))
	(block-type 0)
	(wind-step 0))
    (flet ((get-block ()
	     (prog1 (aref *rock-bits* block-type)
	       (setf block-type (mod (1+ block-type) (length *rock-bits*)))))
	   (get-wind ()
	     (prog1 (aref data wind-step)
	       (setf wind-step (mod (1+ wind-step) (length data)))))
	   (clashp (blk x y)
	     (cond ((< x 0) t) ;; left edge
		   ((loop for line across blk
			    thereis (logbitp 7 (ash line x))) ;; right edge
		    t)
		   ((loop
		      with zone = (- y)
		      for busy in well
		      for i below zone
		      for bix from (- (length blk) zone)
			thereis
			(and (< -1 bix (length blk))
			     (not (zerop (logand busy (ash (aref blk bix) x))))))
		    t)))

	   (establish (blk x y)
	     (assert (<= y 0)) ;; can't move down
	     (loop for i below (+ y (length blk))
		   do (push 0 well))
	     ;;(print well)
	     (decf y (max 0 (+ y (length blk))))
	     (loop for line across blk
		   for head on (nthcdr (- (+ y (length blk))) well)
		   do (setf (car head)
			    (logxor (car head)
				    (ash line x))))))
      
      (loop
	with seen = (make-hash-table :test 'equal)
	for move from 0
	when (< 100 (length well))
	do (let* ((id (list* block-type wind-step
			     (subseq well 0 100)))
		  (saw (gethash id seen)))
	     (when saw
	       (let* ((period-moves (- move saw))
		      (height-then (part-1 data saw))
		      (height (1- (length well)))
		      (period-height (- height height-then)))
		 (multiple-value-bind (div mod) (floor (- blks saw) period-moves)
		   (return (+ (part-1 data (+ mod saw))
			      (* div period-height))))))
	     (setf (gethash id seen) move))
	do (let* ((blk (get-block)) ;; let figure appear
		  (x 2)		    ;; from the left
		  (y 3)) ;; between the rock and the well top, may be negative
	     (loop
	       (let ((dx (ecase (get-wind) (#\< -1) (#\> 1))))
		 (unless (clashp blk (+ x dx) y)
		   (incf x dx)))
	       (if (clashp blk x (1- y))
		   (progn (establish blk x y)
			  (return))
		   (decf y))))))))
