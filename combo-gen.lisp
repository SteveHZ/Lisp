

;; combo-gen.lisp 25/06/20

(defun make-combo-gen-array (x n)
  (let ((my-array (make-array x))
		(wheel (1- x))
		(column-max n)
		(start 1))
	(my-for i (0 wheel)
	  (setf (aref my-array i) (1+ i)))

	#'(lambda ()
		(block my-block
		  (cond ((= wheel -1)
				 (return-from my-block nil))
				((= start 1)
				 (setf start 0))

				((< (aref my-array wheel) n)
				 (incf (aref my-array wheel)))

				(t (my-while (= (aref my-array wheel) column-max)
					 (if (= (decf wheel) -1)
						 (return-from my-block nil))
					 (decf column-max))
				   (incf (aref my-array wheel))
				   
				   (my-for i ((1+ wheel) (1- x))
					 (setf (aref my-array i) (1+ (aref my-array (1- i)))))
				   (setf wheel (1- x))
				   (setf column-max n)))
		  my-array))))

(defun combo-gen-array-test (x n)
  (let ((gen (make-combo-gen-array x n)))
	(my-while (print (funcall gen)))))

(defun make-combo-gen-list (x n)
  (let ((my-list nil)
		(wheel (1- x))
		(column-max n)
		(start 1))
	(my-for i (1 x)
	  (setf my-list (append my-list `(,i))))

	#'(lambda ()
		(block my-block
		  (cond ((= wheel -1)
				 (return-from my-block nil))

				((= start 1)
				 (setf start 0))

				((< (nth wheel my-list) n)
				 (incf (nth wheel my-list)))

				(t (my-while (= (nth wheel my-list) column-max)
					 (if (= (decf wheel) -1)
						 (return-from my-block nil))
					 (decf column-max))
				   (incf (nth wheel my-list))
				   
				   (my-for i ((1+ wheel) (1- x))
					 (setf (nth i my-list)
						   (1+ (nth (1- i) my-list))))
				   (setf wheel (1- x))
				   (setf column-max n)))
		  my-list))))

(defun combo-gen-list-test (x n)
  (let ((gen (make-combo-gen-list x n)))
	(my-while (print (funcall gen)))))

(defun perm-gen (n)
  (let ((my-array (make-array n)))
	(my-for i (0 (1- n))
	  (setf (aref my-array i) (1+ i)))

	(labels ((inner (my-array wheel column-max)
			   (format t "~%~a" my-array)
			   (rotatef (aref my-array (- n 1))
						(aref my-array (- n 2)))
			   (format t "~%~a" my-array)

			   (cond ((< (aref my-array wheel) n)
					  (incf (aref my-array wheel))
					  (inner my-array wheel column-max))
					 ((= (aref my-array wheel) n)
					  (my-while (= (aref my-array wheel) column-max)
						(if (< (decf wheel) 0)
							(return-from perm-gen t))
						(decf column-max))
					  (incf (aref my-array wheel))
					  
					  (my-for i ((1+ wheel) (1- n))
						(setf (aref my-array i) (1+ (aref my-array (1- i)))))
					  (inner my-array (1- n) n)))))
	  (inner my-array (1- n) n))))

;;  http://www.lispology.com/show?1FZG

(defun permutations (bag)
  "Return a list of all the permutations of the input."
  ;; If the input is nil, there is only one permutation:
  ;; nil itself
  (if (null bag) '(nil)
      ;; Otherwise, take an element, e, out of the bag.
      ;; Generate all permutations of the remaining elements,
      ;; And add e to the front of each of these.
      ;; Do this for all possible e to generate all permutations.
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permutations
						   (remove e bag :count 1))))
              bag)))

(defun permutations2 (lst)
  (if (null lst) '(())
	  (let (res)
		(map nil
			 (lambda (x)
			   (setq res
					 (append res 
							 (map 'list
								  (lambda (y) (cons x y)) 
								  (permutations (remove x lst))))))
			 lst)
		res)))

;; https://stackoverflow.com/questions/2087693/how-can-i-get-all-possible-permutations-of-a-list-with-common-lisp/2087771

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
				 append (mapcar (lambda (l) (cons element l))
								(all-permutations (remove element list)))))))

(defun combo-gen (x n)
  (let ((my-array (make-array x)))
	(my-for i (0 (1- x))
	  (setf (aref my-array i) (1+ i)))

	(labels ((inner (my-array wheel column-max)
			   (format t "~%~a" my-array)
			   (cond ((< (aref my-array wheel) n)
					  (incf (aref my-array wheel))
					  (inner my-array wheel column-max))
					 ((= (aref my-array wheel) n)
					  (my-while (= (aref my-array wheel) column-max)
						(if (< (decf wheel) 0)
							(return-from combo-gen t))
						(decf column-max))
					  (incf (aref my-array wheel))
					  
					  (my-for i ((1+ wheel) (1- x))
						(setf (aref my-array i) (1+ (aref my-array (1- i)))))
					  (inner my-array (1- x) n)))))
	  (inner my-array (1- x) n))))

