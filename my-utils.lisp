
(defpackage :my-utils
  (:use :cl)) 

(proclaim '(inline last1 next1))

(defun last1 (lst)
  "Returns the last element in the given LST (from OnLisp p45)"
  (car (last lst)))

(defun next1 (lst)
  "Returns the next element of the given LST (cadr)"
  (car (cdr lst)))

(defun rvassoc (key lst &key (test #'string-equal))
  "Similar to rassoc but on a association list, recursive"
  (cond ((null lst) nil)
		((funcall test key (second (first lst)))
		 (first lst))
		(t (rvassoc key (rest lst) :test test))))

(defun rvassoc2 (key lst &key (test #'string-equal))
  "Similar to rassoc but on a association list, non-recursive"
  (dolist (pair lst 'nil)
	(when (funcall test key (second pair))
	  pair)))
