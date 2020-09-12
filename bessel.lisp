;;; https://github.com/brown131/clmath (adapted)

;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER -*-

;;;; Bessel Functions

;;; Reference
;;;   M. Abramowitz and I. Stegun, eds,
;;;     Handbook of Mathematical Functions,
;;;     National Bureau of Standards, 1964

;;;  (c) Copyright Gerald Roylance 1982, 1984
;;;      All Rights Reserved.
;;;  This file may be distributed noncommercially provided
;;;  that this notice is not removed.

;;; Bugs and Fixes
;;;   Do Y0, Y1 sometime
;;;   Use swamped Miller recurrance relations instead

#|
(in-package "CL-USER")

(eval-when (compile load eval)
  nil)

(eval-when (compile load eval)
  (require "MODULES")
  nil)

(EVAL-WHEN (COMPILE EVAL)
  (module-require "HORNER"))

(module-provide "BESSEL-FUNCTIONS")


|#

;;; Copied from clmath/horner.lisp
(defun poly-expand (var coefs)
  (cond ((null coefs) 0.0)
        ((null (cdr coefs)) (car coefs))
        (t
         `(+ ,(car coefs) (* ,var ,(poly-expand var (cdr coefs)))))))

(defmacro poly (var . coefs)
  (poly-expand var coefs))

;;;; BESSEL FUNCTIONS

;;;; MODIFIED BESSEL FUNCTIONS

;;; modified Bessel functions I0(x) I1(x) ...

;;; What ever happened to the backward recurrence formula???

;;; (exp z cos theta) = I0(z) + 2 (sigma (k = 1 inf) Ik(z) cos (k theta))

;;; I[v+1] = I[v-1] - (2 v / z) I[v]

;;; NBS 9.8.1

(defun bessel-i (order x)			
  (defvar z2)
  (defvar zi)
  (setq order (abs order))
  (cond ((= order 0)
	 (cond ((< (abs x) 3.75)
		(let ((z2 (expt (/ x 3.75) 2)))	;-3.75 <= x <= 3.75
		  (poly z2			;eps < 1.6e-7
			1.0000000 3.5156229
			3.0899424 1.2067492
			0.2659732 0.0360768
			0.0045813)))
	       ((>= x 3.75)			; 3.75 <= x
		(let ((zi (/ 3.75 x)))
		  (* (/ (exp x) (sqrt x))
		     (poly zi
			   0.39894228 0.01328592
			   0.00225319 -.00157565
			   0.00916281 -.02057706
			   0.02635537 -.01647633
			   0.00392377)))	;eps < 1.9e-7 (exp x) / (sqrt x)
		)
	       (T (BESSEL-I ORDER (- X)))))
	((= order 1)
	 (cond ((< (abs x) 3.75)
		(let ((z2 (expt (/ x 3.75) 2)))	;-3.75 <= x <= 3.75
		  (* x				;eps < x * 8e-9
		     (poly z2
			   0.50000000 0.87890594
			   0.51498869 0.15084934
			   0.02658733 0.00301532
			   0.00032411))))
	       ((>= x 3.75)			; 3.75 <= x
		(let ((zi (/ 3.75 x)))
		  (* (/ (exp x) (sqrt x))
		     (poly zi
			   0.39894228 -.03988024
			   -.00362018 0.00163801
			   -.01031555 0.02282967
			   -.02895312 0.01787654
			   -.00420059)))	;eps < 2.2e-7 (exp x) / (sqrt x)
		)
	       (T (- (BESSEL-I ORDER (- X))))))
	(t (- (BESSEL-I (- order 2) x)		; *** doubly recursive
	      (* (/ (float (* 2 (1- order))) x)
		 (BESSEL-I (1- order) x))))))
