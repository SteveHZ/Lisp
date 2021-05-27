
;; ===========
;; 10-27/05/21

(defun match-goals (game)
  (+ (home-score game)
	 (away-score game)))

(defun get-av-match-goals (team games-fn)
  (let ((total 0)
		(games-list (funcall games-fn team)))
	(mapcar #'(lambda (game)
				(+= total (match-goals game)))
			(funcall games-fn team))
	(unless-zerop (length games-list)
		(my-round (/ total (length games-list)) 0.01))))

(defun get-average-goals (leagues games-fn)
  (let ((my-list nil))
	(with-all-teams (team leagues)
	  (push (list (string-upcase (csv-filename league))
				  team
				  (get-av-match-goals team games-fn))
			my-list))
	(sort my-list #'< :key #'third)))

(defun do-av-goals-table (my-list)
  (format-table t my-list
				:column-label '("League" "Team" "Av Goals")
				:column-align '(:left :center :center )))

(defun do-av-summer-goals ()
  (do-av-goals-table
	  (get-average-goals *summer-leagues* #'home-aways)))
(defun do-av-summer-home-goals ()
  (do-av-goals-table
	  (get-average-goals *summer-leagues* #'homes)))
(defun do-av-summer-away-goals ()
  (do-av-goals-table
	  (get-average-goals *summer-leagues* #'aways)))

(defun count-ou-games (games-fn result-fn team)
  (let ((wins 0)
		(games-list (funcall games-fn team)))
	(mapcar #'(lambda (game)
				(when (funcall result-fn game)
				  (incf wins)))
			games-list)
	(calc-percent (length games-list) wins)))

(defun get-ou-wins (leagues games-fn result-fn)
  (let ((my-list nil))
	(with-all-teams (team leagues)
	  (push (list (string-upcase (csv-filename league))
				  team
				  (count-ou-games games-fn result-fn team))
			my-list))
	(sort my-list #'> :key #'third)))

(defun do-ou-wins-table (my-list)
  (format-table t my-list
				:column-label '("League" "Team" "Wins")
				:column-align '(:left :center :center )))

(defun do-summer-over-wins (&optional (n 30))
  (do-ou-wins-table
	  (first-n n (get-ou-wins *summer-leagues* #'home-aways #'is-over))))
(defun do-summer-home-over-wins (&optional (n 30))
  (do-ou-wins-table
	  (first-n n (get-ou-wins *summer-leagues* #'homes #'is-over))))
(defun do-summer-away-over-wins (&optional (n 30))
  (do-ou-wins-table
	  (first-n n (get-ou-wins *summer-leagues* #'aways #'is-over))))

(defun do-summer-under-wins (&optional (n 30))
  (do-ou-wins-table
	  (first-n n (get-ou-wins *summer-leagues* #'home-aways #'is-under))))
(defun do-summer-home-under-wins (&optional (n 30))
  (do-ou-wins-table
	  (first-n n (get-ou-wins *summer-leagues* #'homes #'is-under))))
(defun do-summer-away-under-wins (&optional (n 30))
  (do-ou-wins-table
	  (first-n n (get-ou-wins *summer-leagues* #'aways #'is-under))))

(defun do-ou-wins-by-league (csv-league games-fn result-fn)
  (let ((league-as-list `((,csv-league ""))))  ;; league-name not required here
	(do-ou-wins-table
		(get-ou-wins league-as-list games-fn result-fn))))

(defun do-summer-under-wins-by-league (csv-league)
  (do-ou-wins-by-league csv-league #'home-aways #'is-under))
(defun do-summer-home-under-wins-by-league (csv-league)
  (do-ou-wins-by-league csv-league #'homes #'is-under))
(defun do-summer-away-under-wins-by-league (csv-league)
  (do-ou-wins-by-league csv-league #'aways #'is-under))

(defun do-summer-over-wins-by-league (csv-league)
  (do-ou-wins-by-league csv-league #'home-aways #'is-over))
(defun do-summer-home-over-wins-by-league (csv-league)
  (do-ou-wins-by-league csv-league #'homes #'is-over))
(defun do-summer-away-over-wins-by-league (csv-league)
  (do-ou-wins-by-league csv-league #'aways #'is-over))

