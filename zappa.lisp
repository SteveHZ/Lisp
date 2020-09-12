(defpackage :zappa.macros
(:use :common-lisp))
;; DO NOT USE THIS - USE macros.lisp INSTEAD !!!

(defmacro input (var string)
	`(setf ,var (prompt ,string)))

(defmacro my-with-gensyms ((&rest names) &body body)
	`(let ,(loop for n in names collect `(,n (gensym)))
	,@body))

(defmacro for (var start stop &body body)
;	(let ((gstop (gensym)))
	(my-with-gensyms (gstop)
	`(do ((,var ,start (1+ ,var))
		  (,gstop ,stop))
		((> ,var ,gstop))
			,@body )))

(defmacro while (test &body body)
	`(do ()
		((not ,test))
			,@body))
