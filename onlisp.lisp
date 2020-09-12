(defun fif (if-fn then-fn &optional else-fn)
  #’(lambda (x)
    (if (funcall if-fn x)
        (funcall then-fn x)
        (if else-fn (funcall else-fn x)))))

(defun fint (fn &rest fns)
  (if (null fns)
    fn
    (let ((chain (apply #’fint fns)))
      #’(lambda (x)
        (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
    fn
    (let ((chain (apply #’fun fns)))
      #’(lambda (x)
      (or (funcall fn x) (funcall chain x))))))
