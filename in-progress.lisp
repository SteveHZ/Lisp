
;; Rugby League

(defun home-margin (game margin)
  (> (- (parse-integer (fourth game))
		(parse-integer (fifth game)))
	 margin))

(defun away-margin (game margin)
  (> (- (parse-integer (fifth game))
		(parse-integer (fourth game)))
	 margin))

(defun home-away-margin (margin)
  (remove-if-not #'(lambda (game)
					 (or (home-margin game margin)
						 (away-margin game margin)))
				 *db*))

(defun say-rl-game (game)
  (format t "~%~{~a ~10t~a ~30t v  ~a ~54t~a-~a~}" game))

(defun rl-say (games)
  (mapcar #'(lambda (game)
			  (say-rl-game game))
		  games)
    t)


(defmacro do-rl-say (fn-name games-fn)
  `(defun ,fn-name (team)
	 (rl-say (funcall ,games-fn team))))

(do-rl-say say-rl-homes #'homes)
(do-rl-say say-rl-aways #'aways)
(do-rl-say say-rl-home-aways #'home-aways)

(defmacro do-team-margin (fn-name games-fn)
  `(defun ,fn-name (team margin)
	 (remove-if-not #'(lambda (game)
						(or (home-margin game margin)
							(away-margin game margin)))
					(funcall ,games-fn team))))

(do-team-margin home-team-margin #'homes)
(do-team-margin away-team-margin #'aways)
(do-team-margin home-away-team-margin #'home-aways)

;;; ========================================================================================

(defun goals-scored (team)
  (let ((goals 0)
		(games 0))
	(mapcar #'(lambda (game)
				(if (string-equal team (home-team game))
					(+= goals (home-score game))
					(+= goals (away-score game)))
				(incf games))
			(home-aways team))
	(values goals games)))

;;; ========================================================================================

#|
(defparameter series-ou-results '("Overs" "Unders" "Over Unders" "Under Overs" "Home Overs" "Home Unders" "Away Overs" "Away Unders"))
(defparameter series-ou-fns (list #'series-overs
								  #'series-unders
								  #'series-over-unders
								  #'series-under-overs
								  #'series-overs #'series-unders
								  #'series-overs #'series-unders))
(defparameter series-ou-odds-fns (list #'series-over-odds
									   #'series-under-odds
									   #'series-ou-odds
									   #'series-uo-odds
									   #'series-over-odds #'series-under-odds
									   #'series-over-odds #'series-under-odds))
(defparameter series-games-fns (list #'home-aways #'home-aways #'home-aways #'home-aways
									 #'homes #'homes
									 #'aways #'aways))

(defun do-all-series-calc (series n results results-fns odds-fns games-fns)
  "Produce a list of the best returns from all teams for results given by RESULTS-FNS"

  (let ((my-list nil))
	(with-all-teams (team *leagues*)
	  (mapcar #'(lambda (result result-fn odds-fn games-fn)
				  (multiple-value-bind (stake returns)
					  (calc-team-series team series games-fn result-fn odds-fn)
					(push (list (string-upcase (csv-filename league))
								team
								result
								stake
								(my-round returns 0.01)
								(calc-percent stake returns))
						  my-list)))
			  results results-fns odds-fns games-fns))

	(first-n n (sort my-list #'> :key #'sixth))))

(defun do-all-result-series-calc (series &optional (n 20))
  "Produce a list of the best returns from all teams for each result (Win/Draw/Loss/Win-Loss/Loss-Win)"
  (do-all-series-calc series n series-results series-fns series-odds-fns series-games-fns))

(defun do-all-ou-series-calc (series &optional (n 20))
  "Produce a list of the best returns from all teams for each result (Over/Under/Over-Under/Under-Over)"
  (do-all-series-calc series n series-ou-results series-ou-fns series-ou-odds-fns series-games-fns))


;;; add home-overs, away-overs/unders etc to this - see 2696 series-ou-fns
(defun do-all-ou-series (series &optional (n 20))
  "Produce a list of the best returns from all teams for each result (Over/Under/Over-Under/Under-Over)"
  (do-all-series-table (do-all-ou-series-calc series n)))
|#

(defun make-23-series-test (my-list)
  (let ((idx 0)
		(wins 0)
		(games 0))
	
	#'(lambda (&optional (result ""))
		(labels ((reset-series ()
				   (setf idx 0)
				   (setf wins 0)
				   (setf games 0)
				   (format t "~%Reset : idx = ~a wins = ~a games = ~a" idx wins games)
				   ))

		  (cond ((string-equal result "R")
				 (reset-series))

				((string-equal result "")
				 (nth idx my-list))

				((string-equal result "W")
				 (format t "~%Win > : idx = ~a wins = ~a games = ~a" idx wins games)
				 (if (= idx 0)
					 (incf idx)
					 (decf idx))
				 (incf wins)
				 (incf games)
				 (format t "~%Win > : idx = ~a wins = ~a games = ~a" idx wins games)
				 (if (= wins 2)
					 (reset-series)))
				
				(t (incf idx)
				   (when (= games 2)
					 (setf games 0)
					 (decf wins))
				   (when (= games 1)
					 (incf games))

				   
				   ))

		  (when (null (nth idx my-list))
			(reset-series))
		  (nth idx my-list)))))

(defparameter s246t (make-23-series-test '(2 4 6 8 10 12)))

;;; ========================================================================================
;;; ========================================================================================
;;; ========================================================================================

(defun count-season (games-fn test-fn)
  "Returns a list of all teams sorted by the games returned by FN
   which match the result TEST-FN"

  (labels ((inner (games count)
			 (cond ((null games) count)
				   ((funcall test-fn (car games))
					(inner (rest games) (1+ count)))
				   (t (inner (rest games) count)))))

	(let ((my-list nil))
	  (with-all-teams (team *leagues*)
		(let* ((games (funcall games-fn team))
			   (count (inner games 0)))
 		  (push (list team
					  count
 					  (length games)
					  (calc-percent (length games) count))
				my-list)))

	  (sort my-list #'> :key #'fourth))))

(defun count-season-n (ngames games-fn test-fn)
  "Returns a list of all teams sorted by the games returned by FN
   which match the result TEST-FN"

  (labels ((inner (games count)
			 (cond ((null games) count)
				   ((funcall test-fn (car games))
					(inner (rest games) (1+ count)))
				   (t (inner (rest games) count)))))

	(let ((my-list nil))
	  (with-all-teams (team *leagues*)
		(let* ((games (funcall games-fn team ngames))
			   (count (inner games 0)))
 		  (push (list team
					  count
 					  (length games)
					  (calc-percent (length games) count))
				my-list)))

	  (sort my-list #'> :key #'fourth))))

;;; ========================================================================================

(defun count-ou (games-fn test-fn n goals)
  (count-games-table
   (first-n n (count-season
			   games-fn
			   #'(lambda (game)
				   (funcall test-fn game goals))))))

(defun count-ou-n (games-fn test-fn n goals ngames)
  (count-games-table
   (first-n n (count-season-n
			   ngames
			   games-fn
			   #'(lambda (game)
				   (funcall test-fn game goals))))))

;;; ========================================================================================
(defun count-overs (&key (n 10) (goals 2.5))
  (count-ou #'home-aways #'is-over n goals))
(defun count-home-overs (&key (n 10) (goals 2.5))
  (count-ou #'homes #'is-over n goals))
(defun count-away-overs (&key (n 10) (goals 2.5))
  (count-ou #'aways #'is-over n goals))

(defun count-overs-n (&key (n 10) (goals 2.5) (ngames 6))
  (count-ou-n #'last-six #'is-over n goals ngames))
(defun count-home-overs (&key (n 10) (goals 2.5) (ngames 6))
  (count-ou-n #'last-six-homes #'is-over n goals ngames))
(defun count-away-overs (&key (n 10) (goals 2.5) (ngames 6)) 
  (count-ou-n #'last-six-aways #'is-over n goals ngames))

;;; ========================================================================================
(defun count-unders (&key (n 10) (goals 2.5))
  (count-ou #'home-aways #'is-under n goals))
(defun count-home-unders (&key (n 10) (goals 2.5))
  (count-ou #'homes #'is-under n goals))
(defun count-away-unders (&key (n 10) (goals 2.5))
  (count-ou #'aways #'is-under n goals))


(defun count-unders-n (&key (n 10) (goals 2.5) (ngames 6))
  (count-ou-n #'last-six #'is-under n goals ngames))
(defun count-home-unders (&key (n 10) (goals 2.5) (ngames 6))
  (count-ou-n #'last-six-homes #'is-under n goals ngames))
(defun count-away-unders (&key (n 10) (goals 2.5) (ngames 6))
  (count-ou-n #'last-six-aways #'is-under n goals ngames))

