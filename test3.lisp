(defun test3 (start-list circ-list)
		   (let* ((y (circular circ-list))
				  (x (append (copy-list start-list) y)))			 
			 	 
			 #'(lambda (in)
				 (cond ((string-equal in "L")
						(setf x (rest x)))
					   (t (setf x (append (copy-list start-list) y)))))))