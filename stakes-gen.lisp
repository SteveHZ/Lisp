
;; stakes-gen.lisp 31/07/20 - 01/08/20

(defparameter *selections* 0)
(defparameter *from* 0)

(defun array-sum-previous (array end)
  (reduce #'+ (subseq array 0 end)))

(defun array-init (items stk)
  (let ((arr (make-array items :initial-element 1)))
	(setf (aref arr (1- items))
		  (- stk (array-sum-previous arr (1- items))))
	arr))

(defun re-init-array (base my-array)
  (let ((count 0))
	(my-for num (0 (- base 1))
	  (+= count (aref my-array num)))

	(my-for num (base (- *selections* 2))
	  (setf (aref my-array num) 1)
	  (incf count))

	(setf (aref my-array (- *selections* 1)) (- *from* count)))
  my-array)

(defun run-it (column my-array)
  (loop do
		(cond ((< column (- *selections* 1))
			   (run-it (+ column 1) my-array)
			   
			   (cond ((> column 0)
					  (incf (aref my-array (- column 1)))
					  (if (> (aref my-array (- column 1))
							 (- *from* *selections* -1))
						  (return)
						  (setf my-array (re-init-array column my-array))))))
			  (t (my-while (> (aref my-array column) 0)
				   (print my-array)
				   (decf (aref my-array column))
				   (incf (aref my-array (- column 1))))
				 (return)))
		while (> column 0)))

(defun run2 (my-array)
  (let ((column 0))
	
	#'(lambda ()
			(labels ((inner ()
				   (loop do
						 (format t "~%Inner column = ~a my-array = ~a" column my-array)
						 (read)
						 (cond ((< column (- *selections* 1))
								(format t "~%This column = ~a" column)
								(incf column)
								(inner)
								
								(cond ((> column 0)
									   (format t "~%In here : ~a" my-array)
									   (incf (aref my-array (- column 1)))
									   (if (> (aref my-array (- column 1))
											  (- *from* *selections* -1))
										   (progn
											 (format t "Over there : ~a" my-array)
											 (decf column)
											 (inner))
										   (progn
											 (format t "Re-init : ~a" my-array)
											 (setf my-array (re-init-array column my-array))
											 my-array)))))
							   (t (if (> (aref my-array column) 0)
									  (progn
										(decf (aref my-array column))
										(incf (aref my-array (- column 1)))
										(format t "~%While ~%~a" my-array)))
								  
								  my-array))
						 while (> column 0))))
		  (inner)))))

(defun run3 (my-array)
  (let ((column 1))
	
	#'(lambda ()
		(labels ((inner ()
				   (block zappa
					 (cond ((< column 0) nil)
						   (t (format t "~%Inner column = ~a my-array = ~a" column my-array)
							  (read)
							  (cond ((< column (- *selections* 1))
									 (format t "~%This column = ~a" column)
									 (incf column)
									 (format t " Now column = ~a" column)
									 (inner)
									 
									 (cond ((> column 0)
											
											(incf (aref my-array (- column 1)))
											(format t "~%In here : ~a" my-array)
											(if (> (aref my-array (- column 1))
												   (- *from* *selections* -1))
												(progn
												  (format t "Over there : ~a" my-array)
												  (decf column)
												  (inner))
												(progn
												  
												  (setf my-array (re-init-array column my-array))
												  (format t "Re-init : ~a" my-array)
												  (return-from zappa my-array))))))
									(t (if (> (aref my-array column) 0)
										   (progn
											 (decf (aref my-array column))
											 (incf (aref my-array (- column 1)))
											 (format t "~%While : ~a" my-array)
											 (return-from zappa my-array)
											 )))))))))
		  (inner)))))

(defun run-old (selections from)
  (let ((my-array (array-init selections from)))
	(setf *selections* selections)
	(setf *from* from)
	(run-it 0 my-array)))

(defun run (column my-array)
  #'(lambda ()
	  (loop do
			(cond ((< column (- *selections* 1))
				   (run-it (+ column 1) my-array)
				   
				   (cond ((> column 0)
						  (incf (aref my-array (- column 1)))
						  (if (> (aref my-array (- column 1))
								 (- *from* *selections* -1))
							  (return)
							  (setf my-array (re-init-array column my-array))))))
				  (t (my-while (> (aref my-array column) 0)
					   (decf (aref my-array column))
					   (incf (aref my-array (- column 1))))
					 my-array))
			while (> column 0))))



(defun make-sgen (selections from)
  (let ((my-array (array-init selections from)))
	(setf *selections* selections)
	(setf *from* from)
;;	(run 0 my-array)
	(run3 my-array)
	))

(defun sgen-test (x n)
  (let ((gen (make-sgen x n)))
	(my-while (print (funcall gen)))))


#||
(defun list-sum-previous (list wheel)
  (sum (butlast list wheel)))

(defun list-init (items stk)
  (let ((my-list (make-list items :initial-element 1)))
	(setf (nth (1- items) my-list) (- stk (list-sum-previous my-list 1)))
	my-list))
|#

(defun map-blocks (my-list blocks)
  (mapcar #'(lambda (x)
			  (* x blocks))
		  my-list))

(defun calc (sels stake blocks)
  (let ((gen (make-sgen (length sels) stake)))
	(my-while (print (map-blocks (funcall gen) blocks)))))

(defun calc2 (sels stake blocks)
  (let* ((gen (make-sgen (length sels) stake))
		 (my-list (funcall gen))
		 (list2 (mapcar #'(lambda (x)
							(map-blocks x blocks))
						my-list)))
	list2))
