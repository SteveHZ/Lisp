(use-package :zappa-macros
             :iterate)

(defun my-range (x y)
  (iter (for n from x to y)
    (collect n)))

(defun euler-1 ()
  (apply #'+
         (remove-if-not #'
          (lambda (x)
            (cond ((or (equal 0 (mod x 3))
                       (equal 0 (mod x 5)))
                   x)
                  (t nil)))
          (my-range 3 999))))

;; first version
(defvar lst)
(defun old-make-list (x y &optional (z 1))
  (setf lst nil)
  (my-for-step cnt x y z
    (push cnt lst))
  lst)

(defun old-my-range (x y)
  (loop for n from x to y
        collect n))
