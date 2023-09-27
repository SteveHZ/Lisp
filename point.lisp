(defpackage :point
(:use :common-lisp
	  :zappa.macros))

; 2D point class

(defclass point ()
	((x :accessor x
		:initarg :x
		:initform 0)
	 (y :accessor y
		:initarg :y
		:initform 0)))
		
(defmethod my-describe ((pt point))
	(format t "x value = ~d~%" (x pt))
	(format t "y value = ~d~%" (y pt)))

;(defgeneric move (point))		
(defmethod move ((pt point) addx addy)
	(+= (x pt) addx)
	(+= (y pt) addy))
	
; 3D point class inheriting from point

(defclass point3d (point)
	((z :accessor z
		:initarg :z
		:initform 0)))

(defmethod my-describe :after ((pt point3d))
	(format t "z value = ~d~%" (z pt)))
		
(defmethod move3d ((pt point3d) addx addy addz)
	(move pt addx addy)
	(+= (z pt) addz))

; Usage
; (setf p2 (make-instance 'point :x 1 :y 2))
; (setf p3 (make-instance 'point3d :x 10 :y 20 :z 30))

; (x p2)
; 1
; (z p3)
; 30
; (my-describe p2)
; (my-describe p3)