(defun run-series (s team games-fn result-fn odds-fn)
  (let ((stake 0)
		(returns 0)
		(result ""))
	
	(series-reset s)
	(dolist (game (funcall games-fn team))
	  (let ((current (series-current s)))
		(+= stake current)
		(cond ((funcall result-fn team game)
			   (+= returns (* current (funcall odds-fn team game)))
			   (setf result "W"))
			  (t (setf result "L")))
		(series-update s result)))

	(values stake returns)))

(defun do-series2 (s games-fn result-fn odds-fn)
  "Returns a list of all teams sorted by the returns using series S given the 
   games returned by GAMES-FN which match results from RESULT-FN at odds ODDS-FN"
  
  (let ((my-list nil))
	(with-all-teams (team *leagues*)
	  (multiple-value-bind (stake returns)
		  (run-series s team games-fn result-fn odds-fn)

		(if (> stake 0)
			(push (list (string-upcase (car league))
						team
						stake
						(format nil "~6,2f" returns)
						(my-round (* 100 (/ returns stake)) 0.01))
				  my-list))))

	(sort my-list #'> :key #'fifth)))


(defun do-series-wins-calc2 (series &optional (n 10))
  (first-n n (do-series2 series #'home-aways #'home-away-win-result #'home-away-odds)))

(defun series-table2 (my-list)
  (format-table t my-list
				:column-label '("League" "Team" "Stake" "Return" "Percents")
				:column-align '(:center :left :center :center :center)))

(defun do-series-wins2 (series &optional (n 10))
  (series-table2 (do-series-wins-calc2 series n)))

;;=======================================================
;; above pushes onto list after running entire series
;; below pushes onto list after each game
;; calc-team series is already identical to run-series !!
;;=======================================================

(defun run-series2 (s team games-fn result-fn odds-fn)
  (let ((stake 0)
		(returns 0)
		(result ""))
	
	(series-reset s)
	(dolist (game (funcall games-fn team))
	  (let ((current (series-current s)))
		(+= stake current)
		(cond ((funcall result-fn team game)
			   (+= returns (* current (funcall odds-fn team game)))
			   (setf result "W"))
			  (t (setf result "L")))
		(series-update s result)))

	(values stake returns)))

(defun do-team-series-calc2 (team s games-fn result-fn odds-fn)
  "Returns details of a TEAM with series S, using games returned from GAMES-FN
   which match RESULT-FN at odds ODDS-FN"
  
  (let ((my-list nil)
		(stake 0)
		(returns 0)
		(result ""))

	(series-reset s)
	(dolist (game (funcall games-fn team))
	  (let ((current (series-current s)))
		(+= stake current)
		(cond ((funcall result-fn team game)
			   (+= returns (* current (funcall odds-fn team game)))
			   (setf result "W"))
			  (t (setf result "L")))
		(series-update s result)
		(push (list (date game) (home-team game) (away-team game)
					current result
					(home-odds game) (draw-odds game) (away-odds game)
					stake returns)
			  my-list)))
	(values (reverse my-list)
			stake returns)))

(defun do-team-series2 (team s games-fn result-fn odds-fn)
  (format t "~68tOdds ~83tStake ~89tReturn")

  (multiple-value-bind (my-list stake returns)
	  (do-team-series-calc2 team s games-fn result-fn odds-fn)

	(format t "~{~{~%~a  ~a ~30t v ~a ~55t~a  ~a  ~5,2f ~5,2f ~5,2f  : ~6,2f ~6,2f~}~}" my-list)
	(format t "~%~%Stake  : £~,2f~%Return : £~,2f" stake returns)))

(defun do-team-series-wins2 (team series)
  (do-team-series2 team series #'home-aways #'home-away-win-result #'home-away-odds))
