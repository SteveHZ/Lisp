(defpackage :zappa-runcsv
	(:use :common-lisp :game))
;	(:use :common-lisp :game :zappa-macros))

(load "c:/mine/lisp/macros.lisp")
(load "c:/mine/lisp/game.lisp")

(defvar obj)
(defun runcsv ()

(cl-csv:read-csv #P"C:/Mine/perl/Football/data/fixtures.csv"
	:map-fn #'(lambda (row)
		(new 'game obj
			:league (nth 1 row)
			:home-team (nth 2 row)
			:away-team (nth 3 row))
		(my-describe obj))))
