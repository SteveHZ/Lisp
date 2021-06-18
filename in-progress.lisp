
;; ===========
;; 29/05/21

(defun start-archive (year)
  "Load all data"
  (clear-database)
  (load-archive-leagues year)
  t)

(defun reduce-list (league)
  "Reduce a list of home teams from all games in LEAGUE by removing duplicate items"
  (sort
   (remove-duplicates
	(mapcar #'(lambda (game)
				(home-team game))
			(cdr league))
	:test #'string-equal)
   #'string<))

(defun get-historical-teams ()
  "Create teams-list for each historical league"
  (mapcar #'(lambda (league)
			  (list (string-upcase (csv-filename league))
					(reduce-list league)))
		  *db*))

(defun load-archive-leagues (year)
  (setf *db* nil)
  (load-historical year)

  (dolist (league *leagues*)
	(setf *db* (append *db* (load-archive-league-data league))))
  (setf *teams* (get-historical-teams)))

(defun load-archive-league-data (league)
  (list `(,(csv-filename league) .
		  ,(import-csv
			(format nil "c:/mine/lisp/data/historical/~a.csv" (csv-filename league))))))

;; 14/06.21


(defun do-series-wins-calc (series &optional (n 10))
  (first-n n (do-series series #'home-aways #'home-away-win-result #'home-away-odds)))
(defun do-series-home-wins-calc (series &optional (n 10))
  (first-n n (do-series series #'homes #'home-away-win-result #'home-away-odds)))
(defun do-series-away-wins-calc (series &optional (n 10))
  (first-n n (do-series series #'aways #'home-away-win-result #'home-away-odds)))

(defun do-series-defeats-calc (series &optional (n 10))
  (first-n n (do-series series #'home-aways #'home-away-lost-result #'home-away-lost-odds)))
(defun do-series-home-defeats-calc (series &optional (n 10))
  (first-n n (do-series series #'homes #'home-away-lost-result #'home-away-lost-odds)))
(defun do-series-away-defeats-calc (series &optional (n 10))
  (first-n n (do-series series #'aways #'home-away-lost-result #'home-away-lost-odds)))

(defun do-series-draws-calc (series &optional (n 10))
  (first-n n (do-series series #'home-aways #'home-away-draw-result #'series-draw-odds)))
(defun do-series-home-draws-calc (series &optional (n 10))
  (first-n n (do-series series #'homes #'home-away-draw-result #'series-draw-odds)))
(defun do-series-away-draws-calc (series &optional (n 10))
  (first-n n (do-series series #'aways #'home-away-draw-result #'series-draw-odds)))

(defun do-series-overs-calc (series &optional (n 10))
  (first-n n (do-series series #'home-aways #'series-overs #'series-over-odds)))
(defun do-series-home-overs-calc (series &optional (n 10))
  (first-n n (do-series series #'homes #'series-overs #'series-over-odds)))
(defun do-series-away-overs-calc (series &optional (n 10))
  (first-n n (do-series series #'aways #'series-overs #'series-over-odds)))

(defun do-series-unders-calc (series &optional (n 10))
  (first-n n (do-series series #'home-aways #'series-unders #'series-under-odds)))
(defun do-series-home-unders-calc (series &optional (n 10))
  (first-n n (do-series series #'homes #'series-unders #'series-under-odds)))
(defun do-series-away-unders-calc (series &optional (n 10))
  (first-n n (do-series series #'aways #'series-unders #'series-under-odds)))


(defun series-table (my-list)
  (format-table t my-list
				:column-label '("League" "Team" "Stake" "Return" "Percents")
				:column-align '(:center :left :center :center :center)))

(defun do-series-wins (series &optional (n 10))
  (series-table (do-series-wins-calc series n)))
(defun do-series-home-wins (series &optional (n 10))
  (series-table (do-series-wins-calc series n)))
(defun do-series-away-wins (series &optional (n 10))
  (series-table (do-series-wins-calc series n)))

(defun do-series-defeats (series &optional (n 10))
  (series-table (do-series-defeats-calc series n)))
(defun do-series-home-defeats (series &optional (n 10))
  (series-table (do-series-defeats-calc series n)))
(defun do-series-away-defeats (series &optional (n 10))
  (series-table (do-series-defeats-calc series n)))

(defun do-series-draws (series &optional (n 10))
  (series-table (do-series-draws-calc series n)))
(defun do-series-home-draws (series &optional (n 10))
  (series-table (do-series-draws-calc series n)))
(defun do-series-away-draws (series &optional (n 10))
  (series-table (do-series-draws-calc series n)))

(defun do-series-overs (series &optional (n 10))
  (series-table (do-series-overs-calc series n)))
(defun do-series-home-overs (series &optional (n 10))
  (series-table (do-series-overs-calc series n)))
(defun do-series-away-overs (series &optional (n 10))
  (series-table (do-series-overs-calc series n)))

(defun do-series-unders (series &optional (n 10))
  (series-table (do-series-unders-calc series n)))
(defun do-series-home-unders (series &optional (n 10))
  (series-table (do-series-unders-calc series n)))
(defun do-series-away-unders (series &optional (n 10))
  (series-table (do-series-unders-calc series n)))


;;  xx

(defun do-series (s games-fn result-fn odds-fn)
  "Returns a list of all teams sorted by their returns using series S given the 
   games returned by GAMES-FN which match results from RESULT-FN at odds ODDS-FN"
  
  (let ((my-list nil))
	(with-all-teams (team *leagues*)
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
		
		(when (> stake 0)
		  (push (list (string-upcase (csv-filename league))
					  team
					  stake
					  (format nil "~6,2f" returns)
					  (my-round (* 100 (/ returns stake)) 0.01))
				my-list))))

	(sort my-list #'> :key #'fifth)))

(defun do-series-wins-calc (series)
  (do-series series #'home-aways #'home-away-win-result #'home-away-odds))
(defun do-series-home-wins-calc (series)
  (do-series series #'homes #'home-away-win-result #'home-away-odds))
(defun do-series-away-wins-calc (series)
  (do-series series #'aways #'home-away-win-result #'home-away-odds))

(defun do-series-defeats-calc (series)
  (do-series series #'home-aways #'home-away-lost-result #'home-away-lost-odds))
(defun do-series-home-defeats-calc (series)
  (do-series series #'homes #'home-away-lost-result #'home-away-lost-odds))
(defun do-series-away-defeats-calc (series)
  (do-series series #'aways #'home-away-lost-result #'home-away-lost-odds))

(defun do-series-draws-calc (series)
  (do-series series #'home-aways #'home-away-draw-result #'series-draw-odds))
(defun do-series-home-draws-calc (series)
  (do-series series #'homes #'home-away-draw-result #'series-draw-odds))
(defun do-series-away-draws-calc (series)
  (do-series series #'aways #'home-away-draw-result #'series-draw-odds))

(defun do-series-overs-calc (series)
  (do-series series #'home-aways #'series-overs #'series-over-odds))
(defun do-series-home-overs-calc (series)
  (do-series series #'homes #'series-overs #'series-over-odds))
(defun do-series-away-overs-calc (series)
  (do-series series #'aways #'series-overs #'series-over-odds))

(defun do-series-unders-calc (series)
  (do-series series #'home-aways #'series-unders #'series-under-odds))
(defun do-series-home-unders-calc (series)
  (do-series series #'homes #'series-unders #'series-under-odds))
(defun do-series-away-unders-calc (series)
  (do-series series #'aways #'series-unders #'series-under-odds))

(defun series-table (my-list n)
  (format-table t (first-n n my-list)
				:column-label '("League" "Team" "Stake" "Return" "Percents")
				:column-align '(:center :left :center :center :center)))

(defun do-series-wins (series &optional (n 10))
  (series-table (do-series-wins-calc series) n))
(defun do-series-home-wins (series &optional (n 10))
  (series-table (do-series-wins-calc series) n))
(defun do-series-away-wins (series &optional (n 10))
  (series-table (do-series-wins-calc series) n))

(defun do-series-defeats (series &optional (n 10))
  (series-table (do-series-defeats-calc series) n))
(defun do-series-home-defeats (series &optional (n 10))
  (series-table (do-series-defeats-calc series) n))
(defun do-series-away-defeats (series &optional (n 10))
  (series-table (do-series-defeats-calc series) n))

(defun do-series-draws (series &optional (n 10))
  (series-table (do-series-draws-calc series) n))
(defun do-series-home-draws (series &optional (n 10))
  (series-table (do-series-draws-calc series) n))
(defun do-series-away-draws (series &optional (n 10))
  (series-table (do-series-draws-calc series) n))

(defun do-series-overs (series &optional (n 10))
  (series-table (do-series-overs-calc series) n))
(defun do-series-home-overs (series &optional (n 10))
  (series-table (do-series-overs-calc series) n))
(defun do-series-away-overs (series &optional (n 10))
  (series-table (do-series-overs-calc series) n))

(defun do-series-unders (series &optional (n 10))
  (series-table (do-series-unders-calc series) n))
(defun do-series-home-unders (series &optional (n 10))
  (series-table (do-series-unders-calc series) n))
(defun do-series-away-unders (series &optional (n 10))
  (series-table (do-series-unders-calc series) n))
