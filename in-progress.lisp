;; ===========
;; 11-13/01/21

;;("Dover Athletic" "Shrewsbury" "Sheffield United" "Man City")
;;("Fulham" "Derby" "Dover Athletic" "Eastleigh" "West Ham")

(defparameter *streak-teams*
  '(("Man City" Unders)
	("Sheffield United" Defeats)
	("West Ham" Wins)
	("Morecambe" Wins)
	("Eastleigh" Draws)
	("Sheffield Weds" Defeats)
	("Dover Athletic" Defeats)
	("Fulham" Unders)
	("Lincoln" Wins)
	;;	("Cove Rangers" Defeats)
	;;	("Airdrie Utd" Defeats)
	;;	("Annan Athletic" Defeats)
	;;	("Queens Park" Overs)
	;;	("Clyde" Overs)
	;;	("Peterhead" Unders)
;;	("Shrewsbury" Wins)
;;	("Derby" Defeats)
))

(defun calc-team-spread (team games hcap)
  (let ((my-list nil)
		(handicap (* -1 (abs hcap)))) ;; ensure negative
	(mapcar #'(lambda (game)
				(when (or (and (equal team (home-team game))
							   (> (+ (home-score game) handicap)
								  (away-score game)))
						  (and (equal team (away-team game))
							   (> (+ (away-score game) handicap)
								  (home-score game))))
				  (push game my-list)))
			games)
	my-list))

(defun show-spread (team handicap)
  (reverse
   (calc-team-spread team (home-aways team) handicap)))

(defun calc-spread (team handicap)
  (let ((games (home-aways team)))
	(values (length (calc-team-spread team games handicap))
			(length games))))

(defun calc-spread-list (&key ((:h handicap) 2.5) (n 20))
  (let ((my-list nil))
	(with-all-teams (team *leagues*)
	  (multiple-value-bind (wins games)
		  (calc-spread team handicap)
		(push (list team wins games (calc-percent games wins))
			  my-list)))
	(format t "~{~{~%~a ~18t: ~2d  ~2d  ~5,2f %~}~}"
			(first-n n (sort my-list #'> :key #'fourth)))))

(defun my-streak-team-names ()
  (let ((streak-teams (load-my-streak-teams))
		(my-list nil))
	(mapcar #'(lambda (team)
				(push (first team) my-list))
			streak-teams)
	my-list))

(defun get-streak-games ()
  (let ((my-list nil)
		(my-teams (my-streak-team-names)))
	(dolist (game *fixtures*)
	  (mapcar #'(lambda (team)
				  (when (or (equal team (fhome game))
							(equal team (faway game)))
					(push game my-list)))
			  my-teams))
	my-list))
;;==================================================================================

(defun win-lose-result (team game)
  (if (is-win team game)
	  "W" "L"))

(defun over-under-result (team game ou-func)
  (declare (ignore team))
  (if (funcall ou-func game)
	  "W" "L"))

(defun over-result (team game)
 (over-under-result team game #'is-over))

(defun under-result (team game)
  (over-under-result team game #'is-under))

(defun splice-result-into-list (team game result-fn)
  (append (subseq game 0 5)
		  (cons (funcall result-fn team game)
				(subseq game 6))))

(defun over-result-list (team game)
  (splice-result-into-list team game #'over-result)) 
(defun under-result-list (team game)
  (splice-result-into-list team game #'under-result))

(defun result-list2 (team game)
  "Amend list to transform result column from [H A D] to [W L D] or [W L] for the given TEAM and possible result"
  (if (is-draw game)
	  game
	  (splice-result-into-list team game #'win-lose-result)))

(defun say2 (team games &key (odds nil) (result-fn #'result-list3))
  (mapcar #'(lambda (game)
			  (if odds
				  (say-game-with-odds (funcall result-fn team game))
				  (say-game (funcall result-fn team game))))
		  games)
  t)

#|
;; Macro used to create a function named FN-NAME
;; which will return a list of games from GAMES-FN to output using SAY
(defmacro do-sayx (fn-name games-fn &optional (result-fn #'result-list2))
  `(defun ,fn-name (team &key (odds nil) (result-fn ,result-fn))
	 (say2 team (funcall ,games-fn team) :odds odds :result-fn result-fn)))
(do-sayx say-homesx #'homes)
(do-sayx say-awaysx #'aways)
(do-sayx say-home-awaysx #'home-aways)
(do-sayx say-home-overs3 #'homes #'over-result-list)
|#

(defmacro do-say (fn-name games-fn)
  `(defun ,fn-name (team &key (odds nil))
	 (say team (funcall ,games-fn team) :odds odds)))
(defmacro do-sayn (fn-name games-fn)
  `(defun ,fn-name (team &key (odds nil) (ngames 6))
	 (say team (funcall ,games-fn team ngames) :odds odds)))
(defmacro do-say-ou (fn-name games-fn result-fn)
  `(defun ,fn-name (team &key (odds nil) (result-fn ,result-fn))
	 (say2 team (funcall ,games-fn team) :odds odds :result-fn result-fn)))

;; copy say2 as say
;; then amend do-say(n) to pass :result-fn #'win-lose-result

(do-say-ou say-home-overs2 #'homes #'over-result-list)
(do-say-ou say-away-overs2 #'aways #'over-result-list)
(do-say-ou say-home-away-overs2 #'home-aways #'over-result-list)

(do-say-ou say-home-unders2 #'homes #'under-result-list)
(do-say-ou say-away-unders2 #'aways #'under-result-list)
(do-say-ou say-home-away-unders2 #'home-aways #'under-result-list)

;;==================================================================================

(defmacro nand (&body body)
  `(not (and ,@body)))

;; needs adapting to use (funcall result-fn...)
(defun signal-test (my-list)
  (and (first my-list)
	   (or (nand
			 (second my-list) ; on a streak - if all three are true we have a signal result and three wins, so result is nil otherwise true
			 (third my-list)
			 (fourth my-list))
		   (and (second my-list) ; are we on a continuation of previous streak ?
				(third my-list)
				(fourth my-list)
				(fifth my-list)))))


'' this works
(defun say? (team games &key (odds nil) (result-fn #' result-list))
  (let ((odds-fn (if odds
					 #'say-game-with-odds
					 #'say-game)))
	(mapcar #'(lambda (game)
				(funcall odds-fn (funcall result-fn team game)))
			games)))

#|
;; Ternary operator in lisp eg my $x = ($y > 10) ? y * 2 : y * 10

(defmacro let? (var test a b &body body)
  `(let ((,var
		   (if ,test ,a ,b)))
	 ,@body))
|#

(defun say (team games &key (odds nil) (result-fn #' result-list))
  (let? odds-fn
	  (equal odds nil)
	  #'say-game
	  #'say-game-with-odds
	(mapcar #'(lambda (game)
				(funcall odds-fn (funcall result-fn team game)))
			games))
  t)

(say?? "Stoke" (homes "Stoke"))
