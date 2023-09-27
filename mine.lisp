(load "c:/mine/lisp/macros.lisp")
(load "c:/mine/lisp/math.lisp")
(load "c:/mine/lisp/point.lisp")

;(input user "Please enter user name ")
;(format t "Hello user ~A !!" user)

(defun run ()
	(format t "Point variable :~%")
	(new 'point p :x 1 :y 2)
	(my-describe p)
	(move p 4 5)
	(my-describe p)

	(format t "Point3d variable :~%")
	(new 'point3d p3 :x 10 :y 20 :z 30)
	(my-describe p3)
	(move3d p3 4 5 7)
	(my-describe p3))
