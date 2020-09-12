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

(defmethod my-describe ((self my-poisson))
  (with-accessors ((size size)
                   (results results)) self
    (loop for i from 0 upto size do
          (loop for j from 0 upto size do
                (format t " ~f " (aref results i j)))
          (format t "~%"))))

#|
(defprint-class self my-poisson
  (my-describe self))
|#

