(in-package #:advent-of-code.2020.24)

(defun parse-input ()
  (file-lines (my-input-path "input")))

(defparameter *moves*
  '(("e" . #C(1 0))
    ("se" . #C(0 1))
    ("sw" . #C(-1 1))
    ("w" . #C(-1 0))
    ("nw" . #C(0 -1))
    ("ne" . #C(1 -1 ))))

(assert (= 0 (apply '+ (mapcar 'cdr *moves*))))

(defun read-move (stream)
  (let ((a (or (read-char stream nil)
	       (return-from read-move))))
    (cdr (or (assoc (string a) *moves* :test 'string=)
	     (assoc (map 'string 'identity (list a (read-char stream nil)))
		    *moves* :test 'string=)))))

(defstruct tile
  (neigbours (make-sequence 'simple-vector 6
			    :initial-element nil))
  (is-black nil)
  (black-neighbours 0))

(defun solve (data)
  (let ((field (make-hash-table))
	(black-tiles 0))
    (labels ((tile-at (location)
	       (or (gethash location field)
		   (setf (gethash location field)
			 (make-tile))))

	     (flip (location)
	       (let ((object (tile-at location)))
		 (setf (tile-is-black object)
		       (not (tile-is-black object)))
		 (let ((black-delta (if (tile-is-black object) 1 -1)))
		   (incf black-tiles black-delta)
		   (loop for (nil . d) in *moves*
			 do (incf (tile-black-neighbours (tile-at (+ location d)))
				  black-delta))))))

      (dolist (line data)
	(with-input-from-string (input line)
	  (flip (loop for d = (read-move input) while d sum d))))

      (values black-tiles
	      (flet ((life-step ()
		       (mapc #'flip
			     (loop for location being each hash-key of field using (hash-value tile)
				   for bn = (tile-black-neighbours tile)
				   when (or (and (tile-is-black tile)
						 (or (= 0 bn) (< 2 bn)))
					    (and (not (tile-is-black tile))
						 (= 2 bn)))
				     collect location))))

		(dotimes (i 100 black-tiles) (life-step)))))))

(defun part-1 (&optional (data (parse-input)))
  (nth-value 0 (solve data)))

(defun part-2 (&optional (data (parse-input)))
  (nth-value 1 (solve data)))
