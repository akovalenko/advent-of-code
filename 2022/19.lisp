(in-package #:advent-of-code.2022.19)

;;; Advent of code 2022: day 19
;;; see https://adventofcode.com/2022/day/19

(defun parse-line (line)
  (destructuring-bind (key value) (split-sequence #\: line)
    (let ((i (parse-integer key :start #.(length "Blueprint "))))
      (let ((*package* (find-package :keyword)))
	(cons i (loop for recipe in (split-sequence #\. value :remove-empty-subseqs t)
		      collect (with-input-from-string (in recipe)
				(assert (eq :each (read in)))
				(let ((target (read in)))
				  (read in) ;; robot
				  (read in) ;; costs
				  (cons target
					(loop for read = (read in nil)
					      while read
					      when (integerp read)
						collect (cons read (read in nil))))))))))))

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun res-no (key)
  (position key #(:ore :clay :obsidian :geode)))

(defun blueprint-matrix (tab)
  (let ((m (make-array '(4 4) :initial-element 0)))
    (prog1 m
      (loop for (make . prereq) in tab
	    for n-make = (res-no make)
	    do (loop for (n . res) in prereq
		     for n-res = (res-no res)
		     do (setf (aref m n-make n-res) n))))))

(defun next-states (bpm bots stock horizon)
  (let* ((max-ore (loop for i below 4
			maximize (aref bpm i 0)))
	 (no-mining (<= (* horizon (- max-ore (aref bots 0)))
			(aref stock 0)))) ;; max gold to spend
    #-(and)
    (when no-mining
      (format t "~&no mining for ~a in ~a minutes" (list bots stock) horizon))
    (let ((next-stock (copy-seq stock)))
      (map-into next-stock '+ next-stock bots) ;; collectors collect
      (cons (list bots next-stock)
	    (loop for i below 4
		  for buildable = (loop for r below 4
					for s across stock
					always (>= s (aref bpm i r)))
		  when (and buildable (not (and no-mining (= i 0))))
		    collect (list (let ((next-bots (copy-seq bots)))
				    (incf (aref next-bots i))
				    next-bots)
				  (let ((dim-stock (copy-seq next-stock)))
				    (dotimes (r 4 dim-stock)
				      (decf (aref dim-stock r)
					    (aref bpm i r))))))))))

(defun timely-state<= (s1 s2 horizon)
  (let ((flow1 (replace (load-time-value (vector 0 0 0 0)) (first s1)))
	(stock1 (replace (load-time-value (vector 0 0 0 0)) (second s1)))
	(flow2 (replace (load-time-value (vector 0 0 0 0)) (first s2)))
	(stock2 (replace (load-time-value (vector 0 0 0 0)) (second s2))))
    (dotimes (i 4)
      (let ((ds (min (aref stock1 i) (aref stock2 i)))
	    (df (min (aref flow1 i) (aref flow2 i))))
	(decf (aref stock1 i) ds)
	(decf (aref stock2 i) ds)
	(decf (aref flow1 i) df)
	(decf (aref flow2 i) df)))
    (loop for i below 4
	  always (<= (+ (aref stock1 i)
			(* horizon (aref flow1 i)))
		     (aref stock2 i)))))

(defun prune-states (states horizon)
  (let ((new-states nil))
    (loop for candidate in states
	  unless (loop for compared in new-states
		       thereis (timely-state<= candidate compared horizon))
	    do (push candidate new-states))
    new-states))

(defun max-geodes (blueprint &optional (minutes 24))
  (let ((states (list (list #(1 0 0 0) #(0 0 0 0))))
	(bpm (blueprint-matrix blueprint))
	(minute 0))
    (loop
      (let ((next-states (loop for (b s) in states nconc (next-states bpm b s (- minutes minute 1)))))
	(setf states (prune-states next-states ( - minutes minute 1)))
	(when (> (length states) 15000)
	  (setf states (remove-if (lambda (s) (= 0 (aref (first s) 3))) states)))
	(print (list minute (length states) (first states)))
	(when (= minutes (incf minute))
	  (return (loop for (b s) in states
			maximize (aref s 3))))))))

#- (and)
(defun debug-geodes (blueprint buys &optional (minutes 24))
  (let ((states (list (list #(1 0 0 0) #(0 0 0 0))))
	(bpm (blueprint-matrix blueprint))
	(minute 0))
    (let ((dbg-state (list #(1 0 0 0) #(0 0 0 0))))
      (loop
	(let ((next-states (loop for (b s) in states nconc (next-states bpm b s (- minutes minute 1)))))
	  (print (list minute dbg-state :in states))
	  (print (not (null (member dbg-state states :test 'equalp))))
	  (setf states (prune-states next-states ( - minutes minute )))
	  (dotimes (i 4) ;; increase stock with flow
	    (incf (aref (second dbg-state) i)
		  (aref (first dbg-state) i)))
	  (when-let (buy (pop buys))
	    (incf (aref (first dbg-state) buy))
	    (dotimes (i 4)
	      (decf (aref (second dbg-state) i)
		    (aref bpm buy i))))

	  (when (or (endp buys) (= minutes (incf minute)))
	    (return (loop for (b s) in states
			  maximize (aref s 3)))))))))

(defun part-1 (&optional (data (parse-input)))
  (loop for (n . spec) in data
	sum (* n (max-geodes spec))))

(defun max-geodes-heur (blueprint &optional (minutes 32) (fudge 1000))
  (let ((states (list (list #(1 0 0 0) #(0 0 0 0))))
	(bpm (blueprint-matrix blueprint))
	(minute 0))
    (loop
      (let* ((horizon ( - minutes minute 1))
	     (next-states (loop for (b s) in states nconc (next-states bpm b s horizon))))
	(setf states (prune-states next-states horizon))
	(when (> (length states) fudge)
	  ;; when a combinatorial explosion begins, cut everying
	  ;; except top FUDGE by expected geode count
	  (setf states (subseq (sort states (lambda (s1 s2)
					      (> (+ (aref (second s1) 3) (* horizon (aref (first s1) 3)))
						 (+ (aref (second s2) 3) (* horizon (aref (first s2) 3))))))
			       0 fudge)))
	(print (list minute (length states) (first states)))
	(when (= minutes (incf minute))
	  (return (loop for (b s) in states
			maximize (aref s 3))))))))

(defun part-2 (&optional (data (parse-input)))
  (reduce '* (loop for (n . spec) in (subseq data 0 3)
		   collect (max-geodes-heur spec 32 500))))
