(in-package #:advent-of-code.2021.21)

;;; Advent of code 2021: day 21
;;; see https://adventofcode.com/2021/day/21

(defun parse-line (string)
  (parse-integer string :start (+ 2 (position #\: string))))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))


(defun model-deterministic-dice (p1 p2)
  (let ((die 1)
	(s1 0)
	(s2 0))
    (labels ((roll ()
	       (shiftf die (1+ (mod die 100))))
	     (motion (position score delta)
	       (let ((position (1+ (mod (1- (+ position delta)) 10))))
		 (let ((score (+ score position)))
		   (values position score)))))
      
      (loop while (and (< s1 1000) (< s2 1000))
	    for value = (+ (roll) (roll) (roll))
	    for first = t then (not first)
	    for rolls from 1
	    do (if first
		   (setf (values p1 s1) (motion p1 s1 value))
		   (setf (values p2 s2) (motion p2 s2 value)))
	    finally (return (* (min s1 s2) (* 3 rolls)))))))

(defun part-1 (&optional (data (parse-input)))
  (apply 'model-deterministic-dice data))

(defun gen-offset-weights ()
  (let ((weights (make-sequence 'simple-vector 10 :initial-element 0)))
    (dotimes (i 3 weights)
      (dotimes (j 3)
	(dotimes (k 3)
	  (incf (aref weights (+ 3 i j k))))))))

(defun make-quantum-mega-array ()
  (make-array '(21 21 10 10) :initial-element 0))

(defun make-quantum-mega-model (pos1 pos2)
  (let ((model (make-quantum-mega-array)))
    (setf (aref model 0 0 (1- pos1) (1- pos2))
	  1)
    model))

(defun mega-move-player-tpose (model)
  (loop
    with new-model = (make-quantum-mega-array)
    with winnings = 0
    for weight across (load-time-value (gen-offset-weights))
    for dice from 0
    unless (zerop weight)
      do (dotimes (pos 10)
	   (dotimes (score 21)
	     (let* ((new-pos (mod (+ pos dice) 10))
		    (new-score (+ 1 score new-pos)))
	       (if (< new-score 21)
		   (dotimes (pos2 10)
		     (dotimes (score2 21)
		       (incf (aref new-model score2 new-score pos2 new-pos)
			     (* weight (aref model score score2 pos pos2)))))
		   (incf winnings
			 (* weight
			    (loop for pos2 below 10
				  sum
				  (loop for score2 below 21
					sum
					(aref model score score2 pos pos2)))))))))
    finally (replace (flat-array-alias model)
		     (flat-array-alias new-model))
	    (return winnings)))

(defun model-quantum-game-v2 (pos1 pos2)
  (let ((world (make-quantum-mega-model pos1 pos2))
	(winnings (vector 0 0)))
    (loop
      for player = 0 then (- 1 player)
      until (every 'zerop (flat-array-alias world))
      do (incf (aref winnings player)
	       (mega-move-player-tpose world))
	 finally (return winnings))))

(defun part-2 (&optional (data (parse-input)))
  (reduce 'max (apply 'model-quantum-game-v2 data)))

