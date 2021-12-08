(in-package #:advent-of-code)

(defvar *data-path* #P"~/advent-of-code/")

(defvar *input-suffix* "input")

(defmacro with-test-data ((&optional (suffix "test.input"))
			  &body body)
  `(let ((*input-suffix* ,suffix))
     ,@body))

(defun data-path (rootname)
  (make-pathname :defaults *data-path*
		 :name rootname
		 :type "txt"))

(defmacro my-input-path (&optional (suffix '*input-suffix*))
  (let ((package (package-name *package*)))
    (unless (starts-with-subseq "ADVENT-OF-CODE." package)
      (error "Use my-input-path in AOC subpackages only ~a" package))
    `(data-path (format nil
			,(str (format nil "~a" (subseq package (1+ (position #\. package))))
			      ".~a")
			,suffix))))

(defun only (cell)
  (check-type cell (cons * null) "Single-element list")
  (car cell))

(defun str (&rest args)
  "Concatenate components into string"
  (apply #'concatenate 'string (mapcar 'string args)))

(defun caref (array complex)
  (aref array (realpart complex) (imagpart complex)))

(defun (setf caref) (value array complex)
  (setf (aref array (realpart complex) (imagpart complex))
	value))

(defun file-lines (file &key (remove-empty t))
  (let ((content (read-file-into-string file)))
    (split-sequence #\Newline content
		    :remove-empty-subseqs remove-empty)))

(defun line-groups (lines)
  (delete nil (split-sequence "" lines :test #'string=)))

(defun make-grid-array (line-list)
  (let ((height (length line-list))
	(width (reduce 'max line-list :key 'length)))
    (make-array (list height width)
		:element-type t
		:initial-contents
		(mapcar (lambda (line)
			  (map-into (make-string width :initial-element #\Space)
				    'identity line))
			line-list))))

(defun flat-array-alias (array)
  (make-array (apply '* (array-dimensions array))
	      :displaced-to array
	      :element-type (array-element-type array)))


;;; developer's utilities
#+asdf
(defun ensure-day (day &optional year)
  (setf year (or year
		 (nth-value 5 (decode-universal-time
			       (get-universal-time)))))
  (let ((pathname (asdf:system-source-directory :advent-of-code)))
    (setf pathname
	  (merge-pathnames
	   (make-pathname :directory `(:relative ,(format nil "~4,'0d" year)))
	   pathname))
    (ensure-directories-exist pathname)
    (setf pathname
	  (merge-pathnames (make-pathname :name (format nil "~2,'0d" day)
					  :type "lisp")
			   pathname))
    (handler-case
	(with-open-file
	    (output pathname :direction :output
			     :if-exists :error :if-does-not-exist :create)
	  (write `(in-package ,(make-symbol (year-day-package-name year day)))
		 :stream output :case :downcase)
	  (terpri output)
	  (format output "
;;; Advent of code ~4,'0d: day ~2,'0d
;;; see https://adventofcode.com/~2:*~4,'0d/day/~d

(defun parse-input ()
  (file-lines (my-input-path)))

(defun part-1 (&optional (data (parse-input)))
  data)
" year day)

	  (write-line "" output))
      (file-error ()))
    pathname))
