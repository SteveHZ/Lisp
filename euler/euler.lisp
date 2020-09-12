
;; 01 Multiples of 3 and 5 03/09/20

(defun divide-by-3-or-5 (n)
  (or (zerop (mod n 3))
	  (zerop (mod n 5))))

(defun 01-loop ()
  (loop for i from 1 to 999
		when (divide-by-3-or-5 i)
		sum i))

(defun 01-iterate ()
  (iter (for i from 1 to 999)
	(when (divide-by-3-or-5 i)
	  (sum i))))

;; 02 Even Fibonacci numbers 03/09/20

(defun build-fib ()
  (let ((a 1)
		(b 2))
	
	#'(lambda ()
		(let ((c (+ a b)))
		  (setf a b)
		  (setf b c)))))

(defun sum-even-fib ()
  (let ((sum 2) ; first even Fibonacci number
		(next-num 0) ; to hold each further number
		(next-fib (build-fib)))

	(my-while (> 4000000
				 (setf next-num (funcall next-fib)))
	  (if (evenp next-num)
		  (incf sum next-num)))
	sum))
