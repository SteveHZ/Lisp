
;;; Chapter 4

(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst)
	   (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  (labels ((compare (x y)
			 (and (consp x) ; is x a cons cell ?
				  (or (null y) ; if so, check if y is null OR compare cdrs
					  (compare (cdr x) (cdr y))))))
	(if (and (listp x) (listp y))
		(compare x y)
		(> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
	(dolist (x lst)
	  (let ((val (funcall fn x)))
		(if val (push val acc))))
	(nreverse acc)))
;; Usage
;;(filter #'(lambda (x) (if (numberp x) (1+ x)))	'(a 1 b 2 c 3 d 4)) => (2 3 4 5)

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
			 (let ((rest (nthcdr n source)))
			   (if (consp rest)
				   (rec rest (cons (subseq source 0 n) acc))
				   (nreverse (cons source acc))))))
	(if source (rec source nil) nil)))
;; Usage
;;(group '(a b c d e f g) 2) => ((a b) (c d) (e f) (g))

(defun flatten (x)
  (labels ((rec (x acc)
			 (cond ((null x) acc)
				   ((atom x) (cons x acc))
				   (t (rec (car x) (rec (cdr x) acc))))))
	(rec x nil)))
;; Usage
;;(flatten '(a (b c) ((d e) f))) => (a b c d e f)

(defun mapa-b1 (fn a b &optional (step 1))
  (do ((i a (+ i step))
	   (result nil))
	  ((> i b) (nreverse result))
	(push (funcall fn i) result)))

(defun mapa-b (fn a b &optional (step 1))
  (map-> fn
		 a
		 #'(lambda (x) (> x b))
		 #'(lambda (x) (+ x step))))

(defun map0-n (fn n)
  (mapa-b fn 0 n))
(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i)) ; successive-fn
	   (result nil))
	  ((funcall test-fn i) (nreverse result))
	(push (funcall fn i) result)))

;; Chapter 5

(defun fif (if-fn then-fn &optional else-fn)
  #'(lambda (x)
    (if (funcall if-fn x)
        (funcall then-fn x)
        (if else-fn (funcall else-fn x)))))

#| Doesn't compile in SBCL

(defun fint (fn &rest fns)
  (if (null fns)
	  fn
	  (let ((chain (apply #’fint fns)))
		#’(lambda (x)
			(and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
	  fn
	  (let ((chain (apply #’fun fns)))
		#’(lambda (x)
			(or (funcall fn x) (funcall chain x))))))
|#


