(defpackage :game)

(defclass game ()
	((league :accessor league
		:initarg :league
		:initform 0)
	 (home-team :accessor home-team
		:initarg :home-team
		:initform 0)
	 (away-team :accessor away-team
		:initarg :away-team
		:initform 0)))

(defmethod my-describe ((gm game))
(format t "league = ~a ~a v ~a~%"
 (league gm) (home-team gm) (away-team gm)))

;	(format t "league = ~d~%" (league gm))
;	(format t "home team = ~d~%" (home-team gm))
;	(format t "away team = ~d~%" (away-team gm)))
