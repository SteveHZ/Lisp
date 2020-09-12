(defpackage :my-odds
  (:use :cl :parse-float :zappa-macros :my-poisson)) ;; zappa-macros required for +=

;; Usage
;; (setf odds (make-instance 'my-odds :size 10))
;; (calc-game odds 2.02 0.53)
;; (get-odds odds)

(defclass my-odds ()
  ((size :accessor size
         :initarg :size
         :initform 5)
   (results :accessor results)))

(defmethod calc-game ((self my-odds) home away)
  (my-assert (home :non-negnum) (away :non-negnum))
  (let ((p (make-instance 'my-poisson :size (size self))))
    (setf (results self) (calc-game p home away))))

(defmethod home-win ((self my-odds))
  (let ((total 0))
    (loop for i from 1 upto (size self) do
          (loop for j from 0 to (- i 1) do
                (+= total (parse-float (aref (results self) i j)))))
	(if (= total 0)
		0 (/ 100 total))))

(defmethod away-win ((self my-odds))
  (let ((total 0))
    (loop for i from 1 upto (size self) do
          (loop for j from 0 to (- i 1) do
                (+= total (parse-float (aref (results self) j i)))))
	(if (= total 0)
		0 (/ 100 total))))

(defmethod draw ((self my-odds))
  (let ((total 0))
    (loop for i from 0 upto (size self) do
          (+= total (parse-float (aref (results self) i i))))
	(if (= total 0)
		0 (/ 100 total))))

(defmethod over-2pt5 ((self my-odds) &optional (goals 2.5))
  (let ((total 0))
    (loop for i from 0 upto (size self) do
          (loop for j from 0 to (size self) do
				(if (> (+ i j) goals)
					(+= total (parse-float (aref (results self) i j))))))
	(if (= total 0)
		0 (/ 100 total))))
	
(defmethod under-2pt5 ((self my-odds) &optional (goals 2.5))
  (let ((total 0))
    (loop for i from 0 upto (- goals 0.5) do
          (loop for j from 0 to (- goals 0.5) do
				(if (> (+ i j) goals)
					(+= total (parse-float (aref (results self) i j))))))
	(if (= total 0)
		0 (/ 100 total))))

(defmethod get-odds ((self my-odds))
  (list (home-win self)
        (draw self)
        (away-win self)))

(defmethod vget-odds ((self my-odds))
  (values (home-win self)
          (draw self)
          (away-win self)))

(defmethod my-describe ((self my-odds))
  (format t "~%Home Win : ~,2f ~%Draw     : ~,2f ~%Away Win : ~,2f"
		  (home-win self)
		  (draw self)
		  (away-win self)))

#|
(defprint-class self my-odds
  (my-describe self))
|#
