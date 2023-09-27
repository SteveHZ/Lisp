(load "c:/mine/lisp/macros.lisp")

(defun prompt (str)
	(format t "~%~a >" str)
	(read))

;	ANSI Common Lisp p15
(defun sum-greater (x y z)
	(> (+ x y) z))

;	Adapted from ANSI Common Lisp p16
;	returns  object found obj, not remaining list lst
(defun member-list (obj lst)
	(if (null lst)
		nil
		(if (eql obj (car lst))
			obj
			(member-list obj (cdr lst)))))

;	ANSI Common Lisp p23
(defun show-squares (start end)
	(do ((i start (+ 1 i)))
		((> i end) 'done)
		(format t "~% ~a ~a" i (* i i))))

;	ANSI Common Lisp p24
(defun len (lst)
	(if (null lst)
		0
		(+ (len (cdr lst)) 1)))

;	ANSI Common Lisp p105
(defun single? (lst)
	(and (consp lst) (null (cdr lst))))

(defun append1 (lst obj)
	(append lst (list obj)))

(defun map-int (fn n)
	(let ((acc nil))
		(dotimes ( i n)
			(push (funcall fn i) acc))
		(nreverse acc)))

(defun filter (fn lst)
	(let ((acc nil))
		(dolist (x lst)
			(let ((val (funcall fn x)))
				(if val (push val acc))))
		(nreverse acc)))

(defun most (fn lst)
	(if (null lst)
		(values nil nil)
			(let* ((wins (car lst))
				(max (funcall fn wins)))
			(dolist (obj (cdr lst))
				(let ((score (funcall fn obj)))
					(when (> score max)
						(setf wins obj
							max score))))
			(values wins max))))

;	ANSI Common Lisp p160
(defun my-toplevel ()
	(do ()
		(nil)
		(format t "~%> ")
		(print (eval (read)))))

;	https://lee-phillips.org/lispmath/
;	only calculates to 184
(defun my-fib (x)
	(setf sq5 (sqrt 5))
	(setf first (/ sq5))	; reciprocal
	(setf second (expt (* 0.5 (+ 1 sq5)) x))
	(setf third  (expt (* 0.5 (- 1 sq5)) x))
	(* first (- second third)))

(defun my-fib2 (x)
  (let* ((sq5 (sqrt 5))
         (first (/ 1 sq5))
				 (second (expt (* 0.5 (+ 1 sq5)) x))
				 (third (expt (* 0.5 (- 1 sq5)) x)))
    (* first (- second third))))

(defun reciprocal (x)
	(/ x)) ; (/ 1 x))

(defun fib-test ()
	(for x 1 10
		(format t "~A : ~A~%" x (my-fib x))))

(defun todays-date ()
  (local-time:with-decoded-timestamp (:year y :month m :day d)
									 (local-time:now)
	(values y m d)))
