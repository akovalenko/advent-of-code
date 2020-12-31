(in-package #:advent-of-code.2020.18)

;;; Advent of code 2020: day 18
;;; see https://adventofcode.com/2020/day/18

(defun parse-input ()
  (mapcar 'parse-line (file-lines (my-input-path))))

(defun parse-line (line)
  (with-input-from-string (input line)
    (read (make-concatenated-stream
	   (make-string-input-stream "(")
	   input
	   (make-string-input-stream ")")))))

(defun part-1 (&optional (data (parse-input)))
  (labels ((meval (item)
	     (if (listp item)
		 (let ((left (meval (pop item))))
		   (loop while item
			 do (let ((op (pop item))
				  (right (meval (pop item))))
                              (setf left (funcall op left right))))
                   left)
		 item)))
    (reduce '+ data :key #'meval)))

(defun part-2 (&optional (data (parse-input)))
    (labels ((regroup (item)
               (if (listp item)
                   (let ((terms (split-sequence '* item))
                         (result))
                     (if (endp (cdr terms))
                         (car terms)
                         (dolist (term terms
                                       (progn (pop result)
                                              (nreverse result)))
                           (push term result)
                           (push '* result))))
                   item))

             (ohueval (item)
               (setf item (regroup item))
               (if (listp item)
                   (let ((left (ohueval (pop item))))
                     (loop while item
                           do (let ((op (pop item))
                                    (right (ohueval (pop item))))
                                (setf left (funcall op left right))))
                     left)
                   item)))

      (assert (equal (parse-line "2 + 3 * (4 + 5)") '(2 + 3 * (4 + 5))))
      (assert (equal (ohueval '(2 * (4 * 5) + 3)) 46))

      (reduce '+ data :key #'ohueval)))
