;; page 16
(defun median (list)
  (let* ((len (length list))
		 (mid (/ len 2)))
	(cond ((oddp len)
		   (nth (truncate mid) list))
		  (t (/ (+ (nth mid list)
				   (nth (1- mid) list))
				2)))))

;; page 20

(defun percentile (n list)
  "Find the Nth percentile from sorted LIST"
  (let ((idx (* (/ n 100)
				(length list))))
	(cond ((= (mod idx 1) 0)
		   (/ (+ (nth idx list)
				 (nth (1- idx) list))
			  2))
		  (t (nth (truncate idx) list)))))

(defun percentile (n list)
  "Find the Nth percentile from sorted LIST"
  (let ((idx (ceiling (* (/ n 100)
						 (length list)))))
	(nth (1- idx) list)))

;; page 22
;; Inter Quartile Range
(defun iqr (list)
  (- (percentile 75 list)
	 (percentile 25 list)))


(defun five-number-summary (list)
  `(,(first list)
	,(percentile 25 list)
	,(median list)
	,(percentile 75 list)
	,(car (last list))))

;; page 39

(defun n-choose-x (x n)
  "Calculate number of combinations to choose X from N eg 3 from 5   (n)"
  "Sometimes known as binomial co-efficient. Wriiten as              (x)"
  (/ (factorial n)
	 (* (factorial x)
		(factorial (- n x)))))


(defun binomial-prob (p x n)
  (* (n-choose-x x n)
	 (expt p x)
	 (expt (- 1 p) (- n x))))

(defun traffic-lights (n p)
  (iter (for i from 0 to n)
	(format t "~%Lights : ~d Prob : ~,3f" i (binomial-prob p i n))))

;; page 43
(defun binomial-mean (n p)
  (* n p))

(defun binomial-variance (n p)
  (* n p (- 1 p)))

(defun binomial-std-deviation (n p)
  (sqrt (binomial-variance n p)))
