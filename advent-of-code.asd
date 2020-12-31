(defsystem "advent-of-code"
  :description "Some solutions to adventofcode.com puzzles"
  :author "Anton Kovalenko <anton@sw4me.com>"
  :license "Public domain"
  :depends-on ("alexandria"
               "split-sequence"
	       "cl-ppcre")
  :serial t
  :components ((:file "package")
               ;; The basics: these files can use CL and Alexandria.
               (:file "utils")

	       . #.(cl:loop for day from 1 to 25
		      collect `(:file ,(cl:format nil "2020/~2,'0d" day)))))
