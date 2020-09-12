(defun while-test (x)
  (my-while (> x 0)
    (format t "Loop value : ~d~%" x)
    (decf x)))

(defun for-test (start stop)
  (my-for x start stop
    (format t "Loop value : ~d~%" x)))

(defun step-test (start stop step)
  (my-for-step x start stop step
    (format t "Loop value : ~d~%" x)))

(defun foreach-test (mylist)
	(let ((lineno 2))
	(foreach mylist (lambda (x)
		(format t "~2d : ~a~%" lineno x)
		(+= lineno 2)))))
