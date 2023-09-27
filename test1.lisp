
(defun prompt (str)
	(format t "~%~A >" str)
	(read)
  (print str))
