(load "c:/mine/lisp/macros.lisp")

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
	`(defun ,name ,parameters
		(let ((*test-name* ',name))
		,@body)))

(defun report-result (result form)
	(format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
	result)

(defmacro combine-results (&body forms)
	(my-with-gensyms (result)
		`(let ((,result t))
			,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
			,result)))

(defmacro check (&body forms)
	`(combine-results
		,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test-arithmetic ()
	(combine-results
		(test-+)
		(test-*)))

(deftest test-+ ()
	(check
		(= (+ 1 2) 3)
		(= (+ 1 2 3) 6)
		(= (+ -1 -3) -4)))

(deftest test-* ()
	(check
		(= (* 2 2) 4)
		(= (* 3 3) 9)))



		
		
		
;(defun test-+ ()
;	(and
;		(= (+ 1 2) 3)
;		(= (+ 1 2 3) 6)
;		(= (+ -1 -3) -4)))

;(defun test-* ()
;	(let ((*test-name* 'test-*))
;	(check
;		(= (* 2 2) 4)
;		(= (* 3 3) 9))))

;(defun test-+ ()
;	(format t "~:[Bolox~;pass~] ... ~a~%" (= (+ 1 2) 13) '(= (+ 1 2) 13))
;	(format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
;	(format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

;(defun test-+ ()
;	(report-result (= (+ 1 2) 13) '(= (+ 1 2) 13))
;	(report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
;	(report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

;(defmacro check (form)
;	`(report-result ,form ',form))

;(defun test-+ ()
;	(check (= (+ 1 2) 33))
;	(check (= (+ 1 2 3) 6))
;	(check (= (+ -1 -3) -4)))

;(defun test-+ ()
;	(let ((*test-name* 'test-+))
;	(check
;		(= (+ 1 2) 3)
;		(= (+ 1 2 3) 6)
;		(= (+ -1 -3) -4))))

;(defun test-* ()
;	(let ((*test-name* 'test-*))
;	(check
;		(= (* 2 2) 4)
;		(= (* 3 3) 9))))

		
;(defmacro check (&body forms)
;	`(progn
;		,@(loop for f in forms collect `(report-result ,f ',f))))
