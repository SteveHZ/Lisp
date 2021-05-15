
;; ===========
;; 10/05/21

(defun match-goals (game)
  (+ (home-score game)
	 (away-score game)))

(defun get-av-match-goals (team)
  (let ((total 0)
		(num-games (length (home-aways team))))
	(mapcar #'(lambda (game)
				(+= total (match-goals game)))
			(home-aways team))
	(my-round (/ total num-games) 0.01)))

(defun get-average-goals (leagues)
  (let ((my-list nil))
	(with-all-teams (team leagues)
	  (push (list (string-upcase (csv-filename league))
				  team
				  (get-av-match-goals team))
			my-list))
	(sort my-list #'< :key #'third)))

(defun do-av-goals-table (my-list)
  (format-table t my-list
				:column-label '("League" "Team" "Av Goals")
				:column-align '(:left :center :center )))

(defun do-av-summer-goals ()
  (do-av-goals-table
	  (get-average-goals *summer-leagues*)))
(defun do-av-uk-goals ()
  (do-av-goals-table
	  (get-average-goals *leagues*)))

