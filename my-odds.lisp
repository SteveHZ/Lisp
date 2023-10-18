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
   (results :accessor results)
   (pois :accessor pois)))

(defmethod calc-game ((self my-odds) home away)
  (my-assert (home :non-negnum) (away :non-negnum))
  (setf (pois self) (make-instance 'my-poisson :size (size self)))
  (setf (results self) (calc-game (pois self) home away)))

(defmethod home-win ((self my-odds))
  (let ((total 0))
    (loop for i from 1 upto (size self) do
          (loop for j from 0 to (- i 1) do
                (+= total (parse-float (aref (results self) i j)))))
	(if (= total 0.0)
		0 (/ 100 total))))

(defmethod away-win ((self my-odds))
  (let ((total 0))
    (loop for i from 1 upto (size self) do
          (loop for j from 0 to (- i 1) do
                (+= total (parse-float (aref (results self) j i)))))
	(if (= total 0.0)
		0 (/ 100 total))))

(defmethod draw ((self my-odds))
  (let ((total 0))
    (loop for i from 0 upto (size self) do
          (+= total (parse-float (aref (results self) i i))))
	(if (= total 0.0)
		0 (/ 100 total))))

(defmethod get-over-odds ((self my-odds) &optional (goals 2.5))
  (let ((total 0))
    (loop for i from 0 upto (size self) do
          (loop for j from 0 to (size self) do
				(when (> (+ i j) goals)
					(+= total (parse-float (aref (results self) i j))))))
	(if (= total 0)
		0 (/ 100 total))))

(defmethod over0.5 ((self my-odds))
  (get-over-odds self 0.5))
(defmethod over1.5 ((self my-odds))
  (get-over-odds self 1.5))
(defmethod over2.5 ((self my-odds))
  (get-over-odds self 2.5))
(defmethod over3.5 ((self my-odds))
  (get-over-odds self 3.5))
(defmethod over4.5 ((self my-odds))
  (get-over-odds self 4.5))

(defmethod get-under-odds ((self my-odds) &optional (goals 2.5))
  (let ((total 0))
    (loop for i from 0 upto (- goals 0.5) do
          (loop for j from 0 to (- goals 0.5) do
				(when (< (+ i j) goals)
					(+= total (parse-float (aref (results self) i j))))))
	(if (= total 0)
		0 (/ 100 total))))

(defmethod under0.5 ((self my-odds))
  (get-under-odds self 0.5))
(defmethod under1.5 ((self my-odds))
  (get-under-odds self 1.5))
(defmethod under2.5 ((self my-odds))
  (get-under-odds self 2.5))
(defmethod under3.5 ((self my-odds))
  (get-under-odds self 3.5))
(defmethod under4.5 ((self my-odds))
  (get-under-odds self 4.5))

(defmethod both-sides-yes ((self my-odds))
  (let ((total 0))
	(loop for i from 1 to (size self) do
		  (loop for j from 1 to (size self) do
				(+= total (parse-float (aref (results self) i j)))))
	(if (= total 0)
		0 (/ 100 total))))

(defmethod both-sides-no ((self my-odds))
  (let ((total (parse-float (aref (results self) 0 0))))
	(loop for i from 1 to (size self) do
		  (+= total (parse-float (aref (results self) i 0)))
		  (+= total (parse-float (aref (results self) 0 i))))
	(if (= total 0)
		0 (/ 100 total))))

(defmethod get-odds ((self my-odds))
  (list (home-win self)
        (draw self)
        (away-win self)))

(defmethod vget-odds ((self my-odds))
  (values (home-win self)
          (draw self)
          (away-win self)
		  (over2.5 self)
		  (under2.5 self)))

(defmethod my-describe ((self my-odds))
  (let ((home (home-win self))
		(draw (draw self))
		(away (away-win self))
		(over (over2.5 self))
		(under (under2.5 self))
		(both-yes (both-sides-yes self))
		(both-no (both-sides-no self)))
	(format t "~%Home Win : ~6,2f = ~5,2f%"   home  (unless-zerop home  (/ 100 home)))
	(format t "~%Draw     : ~6,2f = ~5,2f%"   draw  (unless-zerop draw  (/ 100 draw)))
	(format t "~%Away Win : ~6,2f = ~5,2f%~%"   away  (unless-zerop away  (/ 100 away)))
	(format t "~%Over     : ~6,2f = ~5,2f%"   over  (unless-zerop over  (/ 100 over)))
	(format t "~%Under    : ~6,2f = ~5,2f%~%" under (unless-zerop under (/ 100 under)))
	(format t "~%BTTS Yes : ~6,2f = ~5,2f%"   both-yes (unless-zerop both-yes  (/ 100 both-yes)))
	(format t "~%BTTS No  : ~6,2f = ~5,2f%~%"   both-no  (unless-zerop both-no  (/ 100 both-no)))))

(defmethod my-describe2 ((self my-odds) home-expect away-expect &key (print-poisson t))
  (calc-game self home-expect away-expect)
  (when (equal print-poisson t)
	(describe-poisson (pois self) home-expect away-expect))
  (my-describe self))

(defmethod my-describe-ou ((self my-odds) home-expect away-expect)
  (calc-game self home-expect away-expect)
  (format t "~%Over 0.5 : ~6,2f   Under 0.5 : ~6,2f" (over0.5 self) (under0.5 self))
  (format t "~%Over 1.5 : ~6,2f   Under 1.5 : ~6,2f" (over1.5 self) (under1.5 self))
  (format t "~%Over 2.5 : ~6,2f   Under 2.5 : ~6,2f" (over2.5 self) (under2.5 self))
  (format t "~%Over 3.5 : ~6,2f   Under 3.5 : ~6,2f" (over3.5 self) (under3.5 self))
  (format t "~%Over 4.5 : ~6,2f   Under 4.5 : ~6,2f" (over4.5 self) (under4.5 self)))

(defmethod calc-nil-expects ((self my-odds) home-expect away-expect)
  (calc-game self home-expect away-expect)
  (calc-nil-expects (pois self) home-expect away-expect))

(defmethod calc-score-expect ((self my-odds) home-expect away-expect home-score away-score)
  (calc-game self home-expect away-expect)
  (calc-expect (pois self) home-expect away-expect home-score away-score))

(defmethod print-object ((self my-odds) stream)
  (print-unreadable-object (self stream :type t)
	(my-describe self)))

#|
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
(if (< (+ i j) goals)
(+= total (parse-float (aref (results self) i j))))))
(if (= total 0)
0 (/ 100 total))))

|#


