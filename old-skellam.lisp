(load "bessel.lisp")

(defconstant euler (exp 1))

(defun calc-skellam (home away &optional (margin 5))
  (let ((constant (expt euler (- (+ home away))))
        (root-ratio (sqrt (/ home away)))
        (harmonic-mean (sqrt (* home away)))
        (results (make-hash-table :test #'equal)))

    (loop for score-margin from (- margin) upto margin do
          (setf (gethash score-margin results)
                (calc-bessel score-margin constant root-ratio harmonic-mean)))
    results))

(defun calc-bessel (score-margin constant root-ratio harmonic-mean)
  (* (* constant
        (expt root-ratio score-margin))
     (bessel-i (abs score-margin)
               (* 2 harmonic-mean))))

(defun home-win (results &optional (margin 5))
  (let ((total 0))
    (loop for m from 1 upto margin do
          (+= total (gethash m results)))
    (/ 1 total)))

(defun away-win (results &optional (margin 5))
  (let ((total 0))
    (loop for  m from (- margin) upto -1 do
          (+= total (gethash m results)))
    (/ 1 total)))

(defun draw (results)
  (/ 1 (gethash 0 results)))

