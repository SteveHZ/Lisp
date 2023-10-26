

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

(defun loop-test ()
  (loop for i in (list 10 20 30 40 50) collect i))

(defun do-all-ou-series (series &optional (n 20))
  "Produce a list of the best returns from all clubs for each Over/Under result including Over-Unders and Under-Overs"

  (let ((my-list nil))
	(with-all-teams (team *leagues*)
	  (mapcar #'(lambda (result result-fn odds-fn)
				  (multiple-value-bind (stake returns)
					  (calc-team-series team series #'home-aways result-fn odds-fn)
					(push (list (string-upcase (csv-filename league))
								team
								result
								stake
								(my-round returns 0.01)
								(calc-percent stake returns))
						  my-list)))
			  series-ou-results series-ou-fns series-ou-odds-fns))

	(do-all-series-table
		(first-n n (sort my-list #'> :key #'sixth)))))

(defun do-all-series (series n results results-fns odds-fns)
  "Produce a list of the best returns from all clubs for results driven by RESULTS-FNS"

  (let ((my-list nil))
	(with-all-teams (team *leagues*)
	  (mapcar #'(lambda (result result-fn odds-fn)
				  (multiple-value-bind (stake returns)
					  (calc-team-series team series #'home-aways result-fn odds-fn)
					(push (list (string-upcase (csv-filename league))
								team
								result
								stake
								(my-round returns 0.01)
								(calc-percent stake returns))
						  my-list)))
			  results results-fns odds-fns))

	(do-all-series-table
		(first-n n (sort my-list #'> :key #'sixth)))))

(defun do-all-ou-series (series &optional (n 20))
  "Produce a list of the best returns from all clubs for each result (Win/Draw/Loss/Win-Loss/Loss-Win"
  (do-all-series series n series-ou-results series-ou-fns series-ou-odds-fns))

(defun do-all-result-series (series &optional (n 20))
  "Produce a list of the best returns from all clubs for each result (Over/Under/Over-Under/Under-Over)"
  (do-all-series series n series-results series-fns series-odds-fns))

;;; Sort home-aways to do ou-series / uo-series rather than just returns
;;  Adds integer-date to start of each game in list
;;  Needs equivalent of Schwarzian sort - decorate/undecorate


(defun sort-last-six-home-aways (team)
  (sort (mapcar #'(lambda (game)
					(push (integer-date (first game)) game))
				(last-six-home-aways team))
		#'< :key #'first))
#|
(defun sort-last-six-home-awaysx (team)
  (let ((my-list (sort (mapcar #'(lambda (game)
								   (push (integer-date (first game)) game))
							   (last-six-home-aways team))
					   #'< :key #'first)))
	(mapcar (#'lambda (gamex)
			  (pop gamex)
			  gamex)
			my-list)))
|#

;;; =========

;; DSL
;; Calculates the number of games in each league where
;; over-odds were favourite and the game resulted in an over-win

(defun is-over-odds-fav (game)
  (< (over-odds game)
	 (under-odds game)))
(defun is-under-odds-fav (game)
  (< (under-odds game)
	 (over-odds game)))

(defun get-over-favs-wins (league)
  (let* ((count 0)
		 (games (get-league league))
		 (ngames (length games))
		 (over-games (get-over-odds-games games))
		 (nover-games (length over-games)))

	(mapcar #'(lambda (game)
				(when (is-over game)
				  (incf count)))
			over-games)
	(format t "~%League ~a ~12t: " league)
	(format t "Total games : ~d" ngames)
	(format t "~33t Home Over Games : ~d" nover-games)
	(format t "~56t Home Over Wins : ~d" count)
	(format t "~78t Wins : ~d%" (calc-percent nover-games count))))

(defun get-all-over-favs-wins ()
  (mapcar #'(lambda (league)
			  (get-over-favs-wins (csv-filename league)))
		  *uk-leagues*))

;;; ======

(defun get-ou-odds-games (league result-fn)
  (let ((my-list nil))
	(mapcar #'(lambda (game)
				(when (funcall result-fn game)
				  (push game my-list)))
			league)
	my-list))

(defun get-ou-favs-wins (league result-fn odds-fn)
  (let* ((count 0)
		 (games (get-league league))
		 (ngames (length games))
		 (ou-games (get-ou-odds-games games odds-fn))
		 (nou-games (length ou-games)))

	(mapcar #'(lambda (game)
				(when (funcall result-fn game)
				  (incf count)))
			ou-games)
	(format t "~%League ~a ~12t: " league)
	(format t "Total games : ~d" ngames)
	(format t "~33t OU Games : ~d" nou-games)
	(format t "~50t OU Wins : ~d" count)
	(format t "~66t Wins : ~d%" (calc-percent nou-games count))))


(defun get-all-over-favs-wins2 ()
  (mapcar #'(lambda (league)
			  (get-ou-favs-wins (csv-filename league) #'is-over #'is-over-odds-fav))
		  *uk-leagues*))

(defun get-all-under-favs-wins2 ()
  (mapcar #'(lambda (league)
			  (get-ou-favs-wins (csv-filename league) #'is-under #'is-under-odds-fav))
		  *uk-leagues*))


;;; =======

(defun get-new-stake (series-amount odds)
  (if (> odds 1.0)
	  (* series-amount
		 (/ 1 (1- odds)))
	  0.01)) ;; if odds are 1.0 (???) results in division-by-zero errors

(defun calc-new-stake (series-amount odds)
  (ceiling (get-new-stake series-amount odds)))

(defun do-series2 (s games-fn result-fn odds-fn)
  "Returns a list of all teams sorted by their returns using series S given the 
   games returned by GAMES-FN which match results from RESULT-FN at odds ODDS-FN"
  
  (let ((my-list nil))
	(with-all-teams (team *leagues*)
	  (let ((stake 0)
			(returns 0)
			(result ""))
		(series-reset s)
		(dolist (game (funcall games-fn team))
		  (let* ((current (series-current s))
				 (my-odds (funcall odds-fn team game))
				 (my-stake (calc-new-stake current my-odds)))
			(+= stake my-stake)
			(cond ((funcall result-fn team game)
				   (+= returns (* current my-odds))
				   (setf result "W"))
				  (t (setf result "L")))
			(series-update s result)))
		
		(when (> stake 0)
		  (push (list (string-upcase (csv-filename league))
					  team
					  stake
					  (format nil "~6,2f" returns)
					  (calc-percent stake returns))
				my-list))))

	(sort my-list #'> :key #'fifth)))

(defun do-series2-overs-calc (series)
  (do-series2 series #'home-aways #'series-overs #'series-over-odds))
(defun do-series2-unders-calc (series)
  (do-series2 series #'home-aways #'series-unders #'series-under-odds))

(defun do-series2-overs (series &optional (n 10))
  (series-table n (do-series2-overs-calc series)))
(defun do-series2-unders (series &optional (n 10))
  (series-table n (do-series2-unders-calc series)))

;;; =======


(defun get-last-six-overs-calc (team games-fn goals)
  (let ((total 0))
	(mapcar #'(lambda (game)
				(when (> (match-goals game) goals)
				  (incf total)))
			(funcall games-fn team))
	total))

(defun get-last-six-unders-calc (team games-fn goals)
  (let ((total 0))
	(mapcar #'(lambda (game)
				(when (< (match-goals game) goals)
				  (incf total)))
			(funcall games-fn team))
	total))

(defun get-last-six-overs (team &optional (goals 2.5))
  (get-last-six-overs-calc team #'last-six goals))

(defun get-last-six-home-overs (team &optional (goals 2.5))
  (get-last-six-overs-calc team #'last-six-homes goals))

(defun get-last-six-away-overs (team &optional (goals 2.5))
  (get-last-six-overs-calc team #'last-six-aways goals))

(defun get-last-six-home-away-overs (team &optional (goals 2.5))
  (get-last-six-overs-calc team #'last-six-home-aways goals))

(defun get-last-six-unders (team &optional (goals 2.5))
  (get-last-six-unders-calc team #'last-six goals))

(defun get-last-six-home-unders (team &optional (goals 2.5))
  (get-last-six-unders-calc team #'last-six-homes goals))

(defun get-last-six-away-unders (team &optional (goals 2.5))
  (get-last-six-unders-calc team #'last-six-aways goals))

(defun get-last-six-home-away-unders (team &optional (goals 2.5))
  (get-last-six-unders-calc team #'last-six-home-aways goals))
----------------------

(defun get-date-as-string (game)
  (subseq (fdate-time game) 3 8))

(defun get-time-as-string (game)
  (subseq (fdate-time game) 9 14))

(defun get-time (game)
  (let ((game-time (get-time-as-string game)))
	(+ (* 100 (parse-integer (subseq game-time 0 2)))
	   (parse-integer (subseq game-time 3 5)))))

(defun early-odds (date)
  (let ((my-expects nil))
	(mapcar #'(lambda (game)
				(when (and (string-equal (get-date-as-string game) date)
						   (< (get-time game) 1500))
				  (push (do-game-expect (fleague game) (fhome game) (faway game)) my-expects)))
			*fixtures*)
	(print-game-odds (do-odds :games my-expects))))

(defun day-odds (date)
  (let ((my-expects nil))
	(mapcar #'(lambda (game)
				(when (and (string-equal (get-date-as-string game) date)
						   (> (get-time game) 1430)
						   (< (get-time game) 1631))
				  (push (do-game-expect (fleague game) (fhome game) (faway game)) my-expects)))
			*fixtures*)
	(print-game-odds (do-odds :games my-expects))))

(defun late-odds (date)
  (let ((my-expects nil))
	(mapcar #'(lambda (game)
				(when (and (string-equal (get-date-as-string game) date)
						   (> (get-time game) 1650))
				  (push (do-game-expect (fleague game) (fhome game) (faway game)) my-expects)))
			*fixtures*)
	(print-game-odds (do-odds :games my-expects))))

(defun diff-odds-fn (game)
  (abs (- (game-over-odds game)
		  (game-under-odds game))))

(defun do-odds2 (&key (games *expects*) )
  "Calculates odds for each game from goal expectancy values"
  (dolist (game games)
    (let ((ds (make-instance 'my-odds :size 10)))
      (calc-game ds (game-home-goals game) (game-away-goals game))
	  (setf (values (game-home-odds game)
					(game-draw-odds game)
					(game-away-odds game)
					(game-over-odds game)
					(game-under-odds game))
			(vget-odds ds))))
  games)

(defun do-odds3 (&key (games *expects*) (cmp-fn #'game-under-odds))
  (format t "~%")
  (print-game-odds (safe-sort (do-odds2 :games games) #'< :key cmp-fn)))

(defun diff-odds-x (&key (games *expects*))
  (print-game-odds (safe-sort (calc-game-odds :games games) #'< :key #'diff-odds-fn)))

(defun diff-odds (&key (games *expects*) (n 20))
  (print-game-odds (first-n n
			(safe-sort (calc-game-odds :games games) #'> :key #'diff-odds-fn))))


----------------------


(defun early-odds-fn (date)
  (let ((my-expects nil))
	(mapcar #'(lambda (game)
				(when (and (string-equal (get-date-as-string game) date)
						   (< (get-time game) 1500))
				  (push (do-game-expect (fleague game) (fhome game) (faway game)) my-expects)))
			*fixtures*)
	(calc-game-odds :games my-expects)))

(defun early-odds2 (date)
  (print-game-odds
   (early-odds-fn date)))

(defun day-odds-fn (date)
  (let ((my-expects nil))
	(mapcar #'(lambda (game)
				(when (and (string-equal (get-date-as-string game) date)
						   (> (get-time game) 1429)
						   (< (get-time game) 1631))
				  (push (do-game-expect (fleague game) (fhome game) (faway game)) my-expects)))
			*fixtures*)
	(calc-game-odds :games my-expects)))

(defun day-odds2 (date)
  (print-game-odds
   (day-odds-fn date)))

(defun late-odds-fn (date)
  (let ((my-expects nil))
	(mapcar #'(lambda (game)
				(when (and (string-equal (get-date-as-string game) date)
						   (> (get-time game) 1650))
				  (push (do-game-expect (fleague game) (fhome game) (faway game)) my-expects)))
			*fixtures*)
	(calc-game-odds :games my-expects)))

(defun late-odds2 (date)
  (print-game-odds
   (late-odds-fn date)))

(defun date-odds-fn (date)
  (let ((my-expects nil))
	(mapcar #'(lambda (game)
				(when (string-equal (subseq (fdate-time game) 3 8)
									(subseq date 0 5)) ;; to allow date to be entered as either DD/MM or DD/MM/YY
				  (push (do-game-expect (fleague game) (fhome game) (faway game)) my-expects)))
			*fixtures*)
	(calc-game-odds :games my-expects)))

(defun date-odds2 (date)
  (print-game-odds
   (date-odds-fn date)))

(defun date-odds (date)
  (let ((my-expects nil))
	(mapcar #'(lambda (game)
				(when (string-equal (subseq (fdate-time game) 3 8)
									(subseq date 0 5)) ;; to allow date to be entered as either DD/MM or DD/MM/YY
				  (push (do-game-expect (fleague game) (fhome game) (faway game)) my-expects)))
			*fixtures*)
	(print-game-odds (do-odds :games my-expects))))


(defmacro do-odds-fn (fn-name odds-fn)
  `(defun ,fn-name (date)
	 (print-game-odds (funcall ,odds-fn date))))

(do-odds-fn date-odds-mac #'date-odds-fn)
(do-odds-fn early-odds-mac #'early-odds-fn)
(do-odds-fn day-odds-mac #'day-odds-fn)
(do-odds-fn late-odds-mac #'late-odds-fn)

