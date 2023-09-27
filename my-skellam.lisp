(defpackage :my-skellam
  (:use :cl :zappa-math)) ;; zappa-math required for euler

;; Usage
;; (setf sk (make-instance 'my-skellam :margin 6))
;; (calc-skellam sk 2.11 1.46)
;; (home-win sk)

;; OR
;; (setf sk (make-instance 'my-skellam :margin 6))
;; (setf skellam (calc-skellam sk 2.11 1.46))
;; (show-hash skellam)
;; (home-win sk)
;; (get-odds sk)
 
(load "bessel.lisp")  ; bessel-i

(defclass my-skellam ()
  ((margin :reader margin ; reader only gives read-only access instead of :accessor which is read/write
           :initarg :margin
           :initform 5)
   (results :accessor results
            :initform (make-hash-table :test #'equal))))

(defun calc-bessel (score-margin constant root-ratio harmonic-mean)
  (* (* constant
        (expt root-ratio score-margin))
     (bessel-i (abs score-margin)
               (* 2 harmonic-mean))))

(defmethod calc-skellam ((self my-skellam) home away)
  (let ((constant (expt euler (- (+ home away))))
        (root-ratio (sqrt (/ home away)))
        (harmonic-mean (sqrt (* home away))))

    (loop for score-margin from (- (margin self)) upto (margin self) do
          (setf (gethash score-margin (results self))
                (calc-bessel score-margin constant root-ratio harmonic-mean)))
    (results self)))

(defmethod home-win ((self my-skellam))
  (let ((total 0))
    (loop for m from 1 upto (margin self) do
          (+= total (gethash m (results self))))
    (/ 1 total)))

(defmethod away-win ((self my-skellam))
  (let ((total 0))
    (loop for  m from (- (margin self)) upto -1 do
          (+= total (gethash m (results self))))
    (/ 1 total)))

(defmethod spread ((self my-skellam) points)
  (let ((total 0))
	(loop for m from points upto (margin self) do
		  (+= total (gethash m (results self))))
	(/ 1 total)))

(defmethod draw ((self my-skellam))
  (/ 1 (gethash 0 (results self))))

(defmethod sk-show-hash ((self my-skellam))
  "Prints out the given HASH"
  (maphash #'(lambda (k v)
               (format t "~%~a ~20t=> ~a" k v))
           (results self)))

(defmethod show-odds ((self my-skellam))
  (format t "~%Home Win : ~,2f Draw : ~,2f Away Win : ~,2f" (home-win self) (draw self) (away-win self)))

(defmethod get-odds ((self my-skellam))
  (list (home-win self)
        (draw self)
        (away-win self)))

(defmethod vget-odds ((self my-skellam))
  (values (home-win self)
		  (draw self)
		  (away-win self)))


#|
(defmethod show-hash (hash)
  "Prints out the given HASH"
  (maphash #'(lambda (k v)
               (format t "~%~a ~20t=> ~a" k v))
           hash))

(defmethod sk-show-hash ((self my-skellam))
  (show-hash (results self)))
|#

