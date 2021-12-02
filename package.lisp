(defpackage #:advent-of-code
  (:nicknames #:aoc)
  (:use #:common-lisp
	#:split-sequence
	#:alexandria)
  (:export #:data-path
	   #:my-input-path
	   #:with-test-data
	   #:str
	   #:caref
	   #:file-lines
	   #:line-groups

	   #:make-grid-array

	   #:flat-array-alias

	   #:ensure-day
	   #:only))

(in-package #:advent-of-code)

(defun year-day-package-name (year day)
  (format nil "~a.~4,'0d.~2,'0d"
	  (package-name '#:advent-of-code)
	  year day))

(dolist (year '(2018 2019 2020 2021))
  (dotimes (day 25)
    (let ((package-name (year-day-package-name year (1+ day))))
      (unless (find-package package-name)
	(make-package package-name
		      :use (cons '#:advent-of-code (package-use-list '#:advent-of-code))
		      :nicknames (list (format nil "~a.~4,'0d.~2,'0d" "AOC" year (1+ day))))))))
