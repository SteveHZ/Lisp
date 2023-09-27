(defpackage :my-expectdb
  (:use :cl :cl-ppcre))

(defclass my-expectdb ()
  ((expect :initarg :expect
		   :accessor expect
		   :initform (error "Must supply expect value !!"))
   (home   :accessor home-win
		   :initform 0)
   (draw   :accessor draw
		   :initform 0)
   (away   :accessor away-win
		   :initform 0)))

(defmethod incr-stats ((self my-expectdb) result)
  (cond ((equal result "H") (incf (home-win self)))
		((equal result "D") (incf (draw self)))
		((equal result "A") (incf (away-win self)))
		(t (error "Unknown result"))))

(defmethod get-expect-percents ((self my-expectdb))
  (let ((total (+ (home-win self) (draw self) (away-win self))))
	(list (* 100 (/ (home-win self) total))
		  (* 100 (/ (draw self) total))
		  (* 100 (/ (away-win self) total)))))

(defmethod get-expect-odds ((self my-expectdb) percents)
  (mapcar #'(lambda (x)
			  (if (zerop x) 0.0
				  (/ 100 x)))
		  percents))

(defmethod my-describe ((self my-expectdb))
  (let* ((percents (get-expect-percents self))
		 (odds (get-expect-odds self percents)))
	(if (not (ppcre:scan "-" (expect self)))
		(format t " "))
	(format t "~a => Home : ~3d Draw : ~3d Away : ~3d   percents : ~{ ~7,2f ~}  odds : ~{ ~7,2f ~}~%"
			(expect self) (home-win self) (draw self) (away-win self) percents odds)))

#|
(defprint-class self my-expectdb
  (my-describe self))
|#
