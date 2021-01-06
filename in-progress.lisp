=

(defmacro defreturns (name odds-fn game-list)
  `(defun ,name (team)
	 (returns ,odds-fn ,game-list)))

(defreturns home-win-returns2
  #'home-odds (home-wins team))

(defmacro def%-return (name team games-fn returns-fn)
  `(defun ,name (,team)
	 (percentage-return ,games-fn ,returns-fn ,team)))

(def%-return home-win-percentage-return2
  team #'homes #'home-win-returns)

==

19/12/20

(defmacro defreturns (name odds-fn game-list)
  `(defun ,name (team)
	 (returns ,odds-fn ,game-list)))
(defmacro defreturns-ha (name odds-fn game-list)
  `(defun ,name (team)
	 (ha-returns team ,odds-fn ,game-list)))

(defreturns home-win-returns3
  #'home-odds (home-wins team))
(defreturns away-win-returns3
  #'away-odds (away-wins team))
(defreturns-ha home-away-win-returns3
  #'home-away-odds (wins team))
(defreturns-ha home-away-loss-returns3
  #'home-away-lost-odds (defeats team))

== old 147 new 92

27/12/20

(defun returns (odds-fn games-list)
  (labels ((inner (inner-list acc)
			 (if (null inner-list) acc
				 (inner (rest inner-list)
						(+ acc (funcall odds-fn (first inner-list)))))))
    (inner games-list 0)))

(defun ha-returns (team odds-fn games-list)
  (labels ((inner (inner-list acc)
			 (if (null inner-list) acc
				 (inner (rest inner-list)
						(+ acc (funcall odds-fn team (first inner-list)))))))
	(inner games-list 0)))

(defun percentage-return (games-fn results-fn odds-fn team)
  (let ((ngames (length (funcall games-fn team))))
	(cond ((zerop ngames) (values 0 0 0))
		  (t (let ((team-return (returns odds-fn (funcall results-fn team))))
			   (values (/ team-return ngames)
					   team-return
					   ngames))))))

(defun ha-percentage-return (games-fn results-fn odds-fn team)
  (let ((ngames (length (funcall games-fn team))))
	(cond ((zerop ngames) (values 0 0 0))
		  (t (let ((team-return (ha-returns team odds-fn (funcall results-fn team))))
			   (values (/ team-return ngames)
					   team-return
					   ngames))))))

(defun home-win-percentage-return (team)
  (percentage-return #'homes #'home-wins #'home-odds team))
(defun away-win-percentage-return (team)
  (percentage-return #'aways #'away-wins #'away-odds team))

(defun home-loss-percentage-return (team)
  (percentage-return #'homes #'home-defeats #'away-odds team))
(defun away-loss-percentage-return (team)
  (percentage-return #'aways #'away-defeats #'home-odds team))

(defun home-away-win-percentage-return (team)
  (ha-percentage-return #'home-aways #'wins #'home-away-odds team))
(defun home-away-loss-percentage-return (team)
  (ha-percentage-return #'home-aways #'defeats #'home-away-lost-odds team))

(defun draw-percentage-return (team)
  (percentage-return #'home-aways #'draws #'draw-odds team))
(defun home-draw-percentage-return (team)
  (percentage-return #'homes #'home-draws #'draw-odds team))
(defun away-draw-percentage-return (team)
  (percentage-return #'aways #'away-draws #'draw-odds team))

(defun over-percentage-return (team)
  (percentage-return #'home-aways #'home-away-overs #'over-odds team))
(defun home-over-percentage-return (team)
  (percentage-return #'homes #'home-overs #'over-odds team))
(defun away-over-percentage-return (team)
  (percentage-return #'aways #'away-overs #'over-odds team))

(defun under-percentage-return (team)
  (percentage-return #'home-aways #'home-away-unders #'under-odds team))
(defun home-under-percentage-return (team)
  (percentage-return #'homes #'home-unders #'under-odds team))
(defun away-under-percentage-return (team)
  (percentage-return #'aways #'away-unders #'under-odds team))

(defun last-six-win-percentage-return (team)
  (ha-percentage-return #'last-six #'wins-in-last-six #'home-away-odds team))
(defun last-six-home-win-percentage-return (team)
  (percentage-return #'last-six-homes #'wins-in-last-six-homes #'home-odds team))
(defun last-six-away-win-percentage-return (team)
  (percentage-return #'last-six-aways #'wins-in-last-six-aways #'away-odds team))

(defun last-six-loss-percentage-return (team)
  (ha-percentage-return #'last-six #'defeats-in-last-six #'home-away-lost-odds team))
(defun last-six-home-loss-percentage-return (team)
  (percentage-return #'last-six-homes #'defeats-in-last-six-homes #'away-odds team))
(defun last-six-away-loss-percentage-return (team)
  (percentage-return #'last-six-aways #'defeats-in-last-six-aways #'home-odds team))

(defun last-six-draw-percentage-return (team)
  (percentage-return #'last-six #'draws-in-last-six #'draw-odds team))
(defun last-six-home-draw-percentage-return (team)
  (percentage-return #'last-six-homes #'draws-in-last-six-homes #'draw-odds team))
(defun last-six-away-draw-percentage-return (team)
  (percentage-return #'last-six-aways #'draws-in-last-six-aways #'draw-odds team))

(defun last-six-over-percentage-return (team)
  (percentage-return #'last-six #'last-six-overs #'over-odds team))
(defun last-six-home-over-percentage-return (team)
  (percentage-return #'last-six-homes #'last-six-home-overs #'over-odds team))
(defun last-six-away-over-percentage-return (team)
  (percentage-return #'last-six-aways #'last-six-away-overs #'over-odds team))

(defun last-six-under-percentage-return (team)
  (percentage-return #'last-six #'last-six-unders #'under-odds team))
(defun last-six-home-under-percentage-return (team)
  (percentage-return #'last-six-homes #'last-six-home-unders #'under-odds team))
(defun last-six-away-under-percentage-return (team)
  (percentage-return #'last-six-aways #'last-six-away-unders #'under-odds team))

;; =====================================================================================

(defun percents-table (my-list)
  (format-table t my-list
				:column-label '("League" "Team" "Games" "Return" "Percentage")
				:column-align '(:left :center :center :center :center)))

(defun league-percents (fn csv-league)
  "Calculate (loss) percentage returns for each TEAM in LEAGUE"
  (mapcar #'(lambda (team)
			  (multiple-value-bind (percent team-return ngames) (funcall fn team)
				(list (string-upcase csv-league)
					  team
					  ngames
					  (my-round team-return 0.01)
					  (my-round percent 0.01))))
          (get-teams csv-league)))

(defmacro defleague% (fn-name returns-fn)
	`(defun ,fn-name (league)
	   (percents-table
		(safe-sort (league-percents ,returns-fn league) #'> :key #'fifth))))

(defleague% do-win-percents #'home-away-win-percentage-return)
(defleague% do-loss-percents #'home-away-loss-percentage-return)

(defleague% do-home-win-percents #'home-win-percentage-return)
(defleague% do-home-loss-percents #'home-loss-percentage-return)

(defleague% do-away-win-percents #'away-win-percentage-return)
(defleague% do-away-loss-percents #'away-loss-percentage-return)

(defleague% do-draw-percents #'draw-percentage-return)
(defleague% do-home-draw-percents #'home-draw-percentage-return)
(defleague% do-away-draw-percents #'away-draw-percentage-return)

(defleague% do-over-percents #'over-percentage-return)
(defleague% do-under-percents #'under-percentage-return)

(defleague% do-last-six-win-percents #'last-six-win-percentage-return)
(defleague% do-last-six-home-win-percents #'last-six-home-win-percentage-return)
(defleague% do-last-six-away-win-percents #'last-six-away-win-percentage-return)

(defleague% do-last-six-draw-percents #'last-six-draw-percentage-return)

(defleague% last-six-over-percents #'last-six-over-percentage-return)
(defleague% last-six-home-over-percents #'last-six-home-over-percentage-return)
(defleague% last-six-away-over-percents #'last-six-away-over-percentage-return)

(defleague% last-six-under-percents #'last-six-under-percentage-return)
(defleague% last-six-home-under-percents #'last-six-home-under-percentage-return)
(defleague% last-six-away-under-percents #'last-six-away-under-percentage-return)

(defun percents-all (fn)
  "Calculate percentage returns for all teams in all leagues"
  (let ((my-list nil))
    (dolist (league *leagues*)
      (dolist (team (league-percents fn (csv-filename league)))
        (push team my-list)))
    my-list))

(defmacro defall% (fn-name returns-fn)
  `(defun ,fn-name ()
	 (percents-table
	  (safe-sort (percents-all ,returns-fn) #'< :key #'fifth))))

(defall% do-win-percents-all #'home-away-win-percentage-return)
(defall% do-loss-percents-all #'home-away-loss-percentage-return)

(defall% do-home-win-percents-all #'home-win-percentage-return)
(defall% do-home-loss-percents-all #'home-loss-percentage-return)

(defall% do-away-win-percents-all #'away-win-percentage-return)
(defall% do-away-loss-percents-all #'away-loss-percentage-return)

(defall% do-draw-percents-all #'draw-percentage-return)
(defall% do-home-draw-percents-all #'home-draw-percentage-return)
(defall% do-away-draw-percents-all #'away-draw-percentage-return)

(defall% do-over-percents-all #'over-percentage-return)
(defall% do-home-over-percents-all #'home-over-percentage-return)
(defall% do-away-over-percents-all #'away-over-percentage-return)

(defall% do-under-percents-all #'under-percentage-return)
(defall% do-home-under-percents-all #'home-under-percentage-return)
(defall% do-away-under-percents-all #'away-under-percentage-return)

(defall% do-last-six-win-percents-all #'last-six-win-percentage-return)
(defall% do-last-six-home-win-percents-all #'last-six-home-win-percentage-return)
(defall% do-last-six-away-win-percents-all #'last-six-away-win-percentage-return)

(defall% do-last-six-draw-percents-all #'last-six-draw-percentage-return)
(defall% do-last-six-home-draw-percents-all #'last-six-home-draw-percentage-return)
(defall% do-last-six-away-draw-percents-all #'last-six-away-draw-percentage-return)

(defall% do-last-six-loss-percents-all #'last-six-loss-percentage-return)
(defall% do-last-six-home-loss-percents-all #'last-six-home-loss-percentage-return)
(defall% do-last-six-away-loss-percents-all #'last-six-away-loss-percentage-return)

(defall% do-last-six-over-percents-all #'last-six-over-percentage-return)
(defall% do-last-six-home-over-percents-all #'last-six-home-over-percentage-return)
(defall% do-last-six-away-over-percents-all #'last-six-away-over-percentage-return)

(defall% do-last-six-under-percents-all #'last-six-under-percentage-return)
(defall% do-last-six-home-under-percents-all #'last-six-home-under-percentage-return)
(defall% do-last-six-away-under-percents-all #'last-six-away-under-percentage-return)


(defmacro deftop% (fn-name returns-fn)
  `(defun ,fn-name (&optional (n 10))
	 (percents-table
	  (first-n n (safe-sort (percents-all ,returns-fn)
				   #'> :key #'fifth)))))

(deftop% top-win-percents #'home-away-win-percentage-return)
(deftop% top-home-win-percents #'home-win-percentage-return)
(deftop% top-away-win-percents #'away-win-percentage-return)

(deftop% top-loss-percents #'home-away-loss-percentage-return)
(deftop% top-home-loss-percents #'home-loss-percentage-return)
(deftop% top-away-loss-percents #'away-loss-percentage-return)

(deftop% top-draw-percents #'draw-percentage-return)
(deftop% top-home-draw-percents #'home-draw-percentage-return)
(deftop% top-away-draw-percents #'away-draw-percentage-return)

(deftop% top-over-percents #'over-percentage-return)
(deftop% top-home-over-percents #'home-over-percentage-return)
(deftop% top-away-over-percents #'away-over-percentage-return)

(deftop% top-under-percents #'under-percentage-return) 
(deftop% top-home-under-percents #'home-under-percentage-return) 
(deftop% top-away-under-percents #'away-under-percentage-return) 

(deftop% top-last-six-win-percents #'last-six-win-percentage-return)
(deftop% top-last-six-home-win-percents #'last-six-home-win-percentage-return)
(deftop% top-last-six-away-win-percents #'last-six-away-win-percentage-return)

(deftop% top-last-six-loss-percents #'last-six-loss-percentage-return)
(deftop% top-last-six-home-loss-percents #'last-six-home-loss-percentage-return)
(deftop% top-last-six-away-loss-percents #'last-six-away-loss-percentage-return)

(deftop% top-last-six-draw-percents #'last-six-draw-percentage-return)
(deftop% top-last-six-home-draw-percents #'last-six-home-draw-percentage-return)
(deftop% top-last-six-away-draw-percents #'last-six-away-draw-percentage-return)

(deftop% top-last-six-over-percents #'last-six-over-percentage-return)
(deftop% top-last-six-home-over-percents #'last-six-home-over-percentage-return)
(deftop% top-last-six-away-over-percents #'last-six-away-over-percentage-return)

(deftop% top-last-six-under-percents #'last-six-under-percentage-return)
(deftop% top-last-six-home-under-percents #'last-six-home-under-percentage-return)
(deftop% top-last-six-away-under-percents #'last-six-away-under-percentage-return)

;; ===  03/01/21

;; write-returns ??

(defparameter series-list `((,s1 "s1")
							(,s2 "s2")
							(,s3 "s3")
							(,s4 "s4")
							(,s5 "s5")))

(defun write-series (series filename)
  (with-open-file (stream filename
						  :direction :output
						  :if-exists :supersede)
	(dolist (series-pair series-funcs)
	  (destructuring-bind (series-fn series-result) series-pair
		(format stream "~a~%" series-result)
		(dolist (my-list (funcall series-fn series 20))
		  (format stream "~a,~a,~a~%"
				  (first my-list)
				  (second my-list)
				  (fifth my-list))))
	  (format stream "~%"))))

(defun export-series ()
  (dolist (series-pair series-list 'done)
	(destructuring-bind (series series-name) series-pair
	  (let ((filename (format nil "c:/mine/lisp/data/series ~a.csv" series-name)))
		(format t "~%Writing ~a..." filename)
		(write-series series filename)))))

(defparameter series-funcs
  `((,#'do-series-wins-calc "Wins")
	(,#'do-series-home-wins-calc "Home Wins")
	(,#'do-series-away-wins-calc "Away Wins")
	(,#'do-series-draws-calc "Draws")
	(,#'do-series-home-draws-calc "Home Draws")
	(,#'do-series-away-draws-calc "Away Draws")
	(,#'do-series-defeats-calc "Defeats")
	(,#'do-series-home-defeats-calc "Home Defeats")
	(,#'do-series-away-defeats-calc "Away Defeats")))

;; ====

(defparameter returns-funcs
  `((,#'home-away-win-percentage-return "Wins")
	(,#'draw-percentage-return "Draws")
	(,#'home-away-loss-percentage-return "Defeats")
	(,#'over-percentage-return "Overs")
	(,#'under-percentage-return "Unders")))

(defun write-returns (filename return-fns)
  (with-open-file (stream filename
						  :direction :output
						  :if-exists :supersede)
	(dolist (returns-pair return-fns)
	  (destructuring-bind (return-fn result) returns-pair
		(format t "~%Writing ~a..." result)
		(format stream "~a~%" result)
		(dolist (my-list (do-top%-list 20 return-fn))
		  (format stream "~{~a,~a,~a,~a,~a~%~}" my-list)))
	  (format stream "~%"))))

(defun export-returns ()
  (write-returns "c:/mine/lisp/data/returns.csv" returns-funcs)
  t)



;; ===  05/01/21

;; team-streaks-calc ??
;; use series eg '(4 4 2) '(10 10 5) ??
;; may need to write new amended make-series function
;; OR just write series-end? - then reset signal to 0 if nil

(defun do-team-series-calc (team s games-fn result-fn odds-fn)
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

(defun do-team-series (team s games-list result-fn odds-fn)
  (format t "~68tOdds ~83tStake ~89tReturn")

  (multiple-value-bind (my-list stake returns)
	  (do-team-series-calc team s games-list result-fn odds-fn)

	(format t "~{~{~%~a  ~a ~30t v ~a ~55t~a  ~a  ~5,2f ~5,2f ~5,2f  : ~6,2f ~6,2f~}~}" my-list)
	(format t "~%~%Stake  : £~,2f~%Return : £~,2f~%Percentage : ~,2f%" stake returns (* (/ returns stake) 100))))

(defun do-team-series-wins (team series)
  (do-team-series team series #'home-aways #'home-away-win-result #'home-away-odds))

(defun make-short-series (my-list)
  (let ((x (copy-list my-list))
		(count 0))

	#'(lambda (&optional (result ""))
		(if (string-equal result "W")
			(incf count))
		
		(cond ((or (= count 2)
				   (string-equal result "R"))
			   (setf count 0)
			   (setf my-list (copy-list x))
			   (car my-list))
			  ((string-equal result "")
			   (car my-list))
			  ((string-equal result "P")
			   (print x))
			  (t (setf my-list (rest my-list))
				 (cond ((null my-list)
						(setf count 0)
						(setf my-list (copy-list x))))
				 (car my-list))))))
