(load "c:/mine/lisp/macros.lisp")

(defun for-test ()
	(for x 1 10
		(format t "~%~A" x)))
		
(defun ne-test (a b)
	(if (ne a b)
		"Not the fackin same"
		"Fackin same"))