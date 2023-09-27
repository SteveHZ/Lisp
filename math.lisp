(defpackage :zappa-math
  (:use :common-lisp
        :zappa-macros
        :zappa-assert))

(defconstant euler (exp 1)) ; eulers number

(defun my-add (a b)
  (+ a b))

(defun my-subtract (a b)
  (- a b))

(defun my-mult (a b)
  (* a b))

(defun make-mult (a)
  #'(lambda (b) (* a b)))

(defun my-div (a b)
  (float (/ a b)))

;; use as (funcall my-double 24)
(defvar my-double (make-mult 2))
(defvar my-triple (make-mult 3))
(defvar my-quad (make-mult 4))

(defun doublex (x)
  (my-mult x 2))

(defun triple (x)
  (my-mult x 3))

(defun show-double (x)
  (format t "double ~d = ~d" x (funcall my-double x)))

(defun show-quad (x)
  (format t "quadruple ~d = ~d" x (funcall my-quad x)))

(defun perms (a b)
  (let ((top 1)(bot 1))
    (loop for count from b downto (+ (- b a) 1) do
      (setf top (* top count)))
    (loop for count from 2 to a do
      (setf bot (* bot count)))
    (/ top bot)))

(defun doubles (x) (perms 2 x))
(defun triples (x) (perms 3 x))
(defun quadruples (x) (perms 4 x))

(defun equal-to (x)
  (if (eql x 3)
      '("Equal to 3")
      '("Not equal to 3")))

(defun greater-than (x)
  (if (>= x 10)
      '("Greater than or equal to 10")
      '("Less than 10")))

(defun prompt (string)
  (format t "~a : " string)
  (format nil "~a" (read-line)))

(defun get-number ()
  (let ((a (prompt "Please enter a number")))
    (format t "The number entered was ~d" a)
    a))

(defun show-multiples (start end multiplier)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i multiplier))))

(defun my-max (x y)
  (if (> x y)
      x y))

(defun my-min (x y)
  (if (< x y)
       x y))

;; call as (my-cmp '> 12 26) or (my-cmp '< 12 26)
(defun my-cmp (cmp x y)
  (if (funcall cmp x y)
      x y))

(defun average-simple (x y)
  (/ (+ x y) 2))

(defun average (&rest args)
  (/ (apply #'+ args)
     (length args)))

(defun 5% (amount)
  (* 0.05 amount))

(defun 5%- (amount)
  (* 0.95 amount))

(defun 5%+ (amount)
  (* 1.05 amount))

(defun percent (fraction amount)
  (* (/ fraction 100)
     amount))
(defun % (fraction amount)
  (* (/ fraction 100)
     amount))
(defun %- (fraction amount)
  (* (/ (- 100 fraction) 100)
     amount))

(defun fib (n)
  (cond ((equal n 0) 1)
        ((equal n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

;;	https://lee-phillips.org/lispmath/
;;  algorithim is from Knuth
;;	only calculates to 184
(defun my-fib2 (x)
  (let* ((sq5 (sqrt 5))
         (first (/ 1 sq5))
		 (second (expt (* 0.5 (+ 1 sq5)) x))
		 (third (expt (* 0.5 (- 1 sq5)) x)))
    (* first (- second third))))

(defun reciprocal (x)
  (/ x)) ; (/ 1 x))

(defun sum (args)
  (apply #'+ args))

;; USE sum above instead
;; apply consistently uses less processor cycles than reduce - TESTED
(defun reduce-sum (lst)
  (reduce #'+ lst))

(defun ^ (x y)
  (expt x y))

;; thinkbayes

(defun mean (x)
  (/ (apply #'+ x)
     (length x)))

(defun variance (args)
  (let ((mean (mean args)))
    (float
     (/ (sum (mapcar
              #' (lambda (x)
                   (expt (- x mean) 2))
				 args))
        (1- (length args))))))

(defun normalise (args)
  (mapcar #'(lambda (x)
              (//= x (sum args))) args))

(defun std-deviation (args)
  (sqrt (variance args)))

(defun bayes (prob-a prob-b-given-a prob-b)
  (/ (* prob-a prob-b-given-a)
     prob-b))

;; Statistics for Dummies p16
(defun medianx (list)
  (let* ((len (length list))
		 (mid (/ len 2)))
	(cond ((oddp len)
		   (nth (truncate mid) list))
		  (t (/ (+ (nth mid list)
				   (nth (1- mid) list))
				2)))))

;; Statistics for Dummies p 20
(defun whole-numberp (n)
  (= (mod n 1) 0))

(defun not-whole-numberp (n)
  (not (whole-numberp n)))

(defun percentile (n list)
  "Find the Nth percentile from sorted LIST"
  (let ((idx (* (/ n 100)
				(length list))))
	(cond ((not-whole-numberp idx)
		   (nth (truncate idx) list))
		  (t (/ (+ (nth idx list)
				   (nth (1- idx) list))
				2)))))

(defun median (list)
  (percentile 50 list))

;; Statistics for Dummies p 22
;; Inter Quartile Range
(defun iqr (list)
  (- (percentile 75 list)
	 (percentile 25 list)))

(defun factorial (n)
  (my-assert (n :non-negint))
  (labels ((fact (num)
             (if (= 0 num) 1
                 (* num (fact (1- num))))))
    (fact n)))

;; Statistics for Dummies p 39
(defun n-choose-x (x n)
  "Calculate number of combinations to choose X from N eg 3 from 5   (n)
   Sometimes known as binomial co-efficient. Wriiten as              (x)"
  (/ (factorial n)
	 (* (factorial x)
		(factorial (- n x)))))

(defun binomial-prob (p x n)
  (* (n-choose-x x n)
	 (expt p x)
	 (expt (- 1 p) (- n x))))

;; savings

(defun int-per-month (amount rate)
  (float (* (* amount (/ rate 100))
            (/ 30 365))))

(defun int-per-year (amount rate)
  (* amount (/ rate 100)))

(defun reg-saver-return (amount rate &optional (months 12))
  (let ((invested 0))
    (my-for month (1 months)
      (+= invested amount)
      (+= invested (int-per-month invested rate)))
    invested))

(defun reg-saver-interest (amount rate &optional (months 12))
  (- (reg-saver-return amount rate months)
     (* amount months)))

;; ?? should use (push (list ...)) rather than backquotes
(defun reg-saver-make-list (amount rate &optional (months 12))
  (let ((invested 0)
        (monthly-interest 0)
        (total-interest 0)
        (rs-list nil))
    (my-for month (1 months)
      (+= invested amount)
      (setf monthly-interest (int-per-month invested rate))
      (+= invested monthly-interest)
      (+= total-interest monthly-interest)
      (push `(,month ,amount ,invested ,monthly-interest ,total-interest) rs-list))
    (reverse rs-list)))

(defun reg-saver-list (amount rate &optional (months 12))
  (format t "~{~{ ~2d : Amount : ~7,2f Invested : ~7,2f Interest : ~5,2f Total : ~6,2f ~}~%~}"
          (reg-saver-make-list amount rate months)))

(defun non-negp (n)
  (and (integerp n)
       (>= n 0)))
(defun non-neg-floatp (n)
  (and (numberp n)
       (>= n 0)))
(defun posintp (n)
  (and (integerp n)
       (> n 0)))
(defun posfloat (n)
  (and (numberp n)
       (> n 0)))

;; adapted from statistics.lisp
(defun my-factorial (n)
  (if (zerop n) 1
	  (* n (factorial (1- n)))))

;; http://www.codecodex.com/wiki/Round_a_number_to_a_specific_decimal_place#Common_Lisp
(defun round-to (number precision &optional (what #'round))
  (let ((div (expt 10 precision)))
	(/ (funcall what (* number div)) div)))

;; To floor a number to steps of 0.5,
;; multiply by 2, floor then divide result by 2 (1 / 0.5)
;; For 0.25, multiply and divide by 4 (1 / 0.25)
(defun my-floor (n &optional (divisor 1))
  (let ((x (/ 1.0 divisor)))
	(/ (floor (* x n)) x)))

(defun my-round (n &optional (divisor 1))
  (let ((x (/ 1.0 divisor)))
	(/ (round (* x n)) x)))

(defun my-ceil (n &optional (divisor 1))
  (let ((x (/ 1.0 divisor)))
	(/ (ceiling (* x n)) x)))

(defun my-roundto (n &key (divisor 1) (func #'round))
  (let ((x (/ 1.0 divisor)))
	(/ (funcall func (* x n)) x)))

(defun acca (stake odds)
  (my-round (* stake (apply #'* odds)) 0.01))

(defun is-leap-year (y)
  (and (zerop (mod y 4))
	   (or (zerop (mod y 400))
		   (not (zerop (mod y 100))))))

(defun calc-percent (stake return)
  (if (= stake 0) 0
	  (my-round (* 100 (/ return stake)) 0.01)))

(defun xor=1 (flag)
  (logxor flag 1))

(defun within-range (a b c)
  (and (>= a b)
	   (<= a c)))

(defun between (var low high)
  (and (> var low)
	   (< var high)))

(defun between= (var low high)
  (and (>= var low)
	   (<= var high)))
