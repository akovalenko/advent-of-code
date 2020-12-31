(in-package #:advent-of-code.2020.04)

;;; Advent of code 2020: day 04
;;; see https://adventofcode.com/2020/day/4

(defun parse-input ()
  (line-groups (file-lines (my-input-path) :remove-empty nil)))

(defun group-fields (group)
  (loop for line in group
	nconc (loop for item in (split-sequence #\Space line)
		    collect (first (split-sequence #\: item :count 1)))))

(defun valid-passport (fields)
  (and (subsetp '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")
		fields :test 'equal)
       (subsetp fields
		'("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid")
		:test 'equal)))

(defun part-1 (&optional (data (parse-input)))
  (count-if 'valid-passport (mapcar 'group-fields data)))

(defun group-fields-v2 (group)
  (loop for line in group
	nconc (loop for item in (split-sequence #\Space line)
		    for colon = (position #\: item)
		    collect (cons (subseq item 0 colon)
				  (subseq item (1+ colon))))))

(defun valid-passport-v2 (fields)
  (and
   (subsetp '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")
	    (mapcar 'car fields) :test 'equal)
   (subsetp (mapcar 'car fields)
	    '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid")
	    :test 'equal)
   (loop for (field . value) in fields
	 always (valid-field field value))))

(defun valid-field (field value)
  (cond ((string= field "byr")
	 (<= 1920 (parse-integer value) 2002))
	((string= field "iyr")
	 (<= 2010 (parse-integer value) 2020))
	((string= field "eyr")
	 (<= 2020 (parse-integer value) 2030))
	((string= field "hgt")
	 (cond ((ends-with-subseq "in" value)
		(<= 59 (parse-integer value :end (+ -2 (length value)))
		    76))
	       ((ends-with-subseq "cm" value)
		(<= 150 (parse-integer value :end (+ -2 (length value)))
		    193))))
	((string= field "hcl")
	 (and (char= #\# (char value 0))
	      (every (rcurry 'find "0123456789abcdef") (subseq value 1))))
	((string= field "ecl")
	 (member value '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")
		 :test 'equal))
	((string= field "pid")
	 (and (= 9 (length value))
	      (every 'digit-char-p value)))
	((string= field "cid") t)))

(defun part-2 (&optional (data (parse-input)))
  (count-if 'valid-passport-v2 (mapcar 'group-fields-v2 data)))
