
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

