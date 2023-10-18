(defpackage :my-poisson
  (:use :cl :zappa-math :zappa-assert)) ;; zappa-math required for euler

;; Usage
;; (setf p (make-instance 'my-poisson :size 5))
;; (calc-game p 2.02 0.53)
;; (my-describe p)

(defclass my-poisson ()
  ((size :accessor size
         :initarg :size
         :initform 5)
   (results :accessor results)))

(defmethod initialize-instance :after ((self my-poisson) &key)
  (let ((my-size (+ 1 (size self))))
    (setf (results self) (make-array `(,my-size ,my-size) :initial-element 0))))

(defun poisson (expect score)
  (/ (* (expt expect score)
        (expt euler (- expect)))
     (factorial score)))

(defun calc-poisson (home-expect away-expect home-score away-score)
  (* (poisson home-expect home-score)
     (poisson away-expect away-score)
     100))

(defmethod calc-game ((self my-poisson) home-expect away-expect)
  (my-assert (home-expect :non-negnum) (away-expect :non-negnum))
  (with-accessors ((size size)
                   (results results)) self
    (loop for home from 0 upto size do
          (loop for away from 0 upto size do
                (setf (aref results home away)
                      (format nil "~6,3f" (calc-poisson home-expect away-expect home away)))))
    results))

(defmethod calc-nil-expects ((self my-poisson) home-expect away-expect)
  (aref (results self) 0 0))

(defmethod calc-expect ((self my-poisson) home-expect away-expect home-score away-score)
  (aref (results self) home-score away-score))

(defmethod my-describe ((self my-poisson))
  (with-accessors ((size size)
                   (results results)) self
	(format t "~25t AWAY~%")
	(format t "~12t~a~20t~a~28t~a~36t~a~44t~a~52t~a" 0 1 2 3 4 5)
	(format t "~%-------------------------------------------------------~%")
	
    (let ((str " HOME      "))
	  (loop for i from 0 upto size do
			(format t " ~a ~2a | " (char str i) i)
			(loop for j from 0 upto size do
				  (format t " ~f " (aref results i j)))
			(format t "~%"))))
  (format t "~%"))

(defgeneric describe-poisson (self home-expect away-expect) (:documentation "describe-poisson"))
(defmethod describe-poisson ((self my-poisson) home-expect away-expect)
  (calc-game self home-expect away-expect)
  (my-describe self))

(defmethod print-object ((self my-poisson) stream)
  (print-unreadable-object (self stream :type t)
	(my-describe self)
	t))

