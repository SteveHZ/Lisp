;; ===  05/01/21

;; team-streaks-calc ??
;; use series eg '(4 4 2) '(10 10 5) ??
;; may need to write new amended make-series function
;; OR just write series-end? - then reset signal to 0 if nil

(defun make-streak-series (my-list)
  (let ((x (copy-list my-list))
		(count 0))

	#'(lambda (&optional (result ""))
		
		(cond ((or (string-equal result "L")
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
						(setf my-list (copy-list x))))
				 (car my-list))))))

(defun do-streak (s games-fn result-fn odds-fn)
  "Returns a list of all teams sorted by their returns using series S given the 
   games returned by GAMES-FN which match results from RESULT-FN at odds ODDS-FN"
  
  (let ((my-list nil))
	(with-all-teams (team *leagues*)
	  (let ((stake 0)
			(returns 0)
			(signal-result 0))
		(series-reset s)

		(dolist (game (funcall games-fn team))
		  (let ((current (series-current s)))
			(if (> signal-result 0)
				(+= stake current))
			(cond ((funcall result-fn team game)
				   (if (> signal-result 0)
					   (progn
						 (+= returns (* current (funcall odds-fn team game)))
						 (series-update s "W")))
				   (incf signal-result)
				   (if (> signal-result 3)
					   (setf signal-result 0)))
				  (t (series-update s "R")
					 (setf signal-result 0)))))
		
		(if (> stake 0)
			(push (list (string-upcase (csv-filename league))
						team
						stake
						(format nil "~6,2f" returns)
						(my-round (* 100 (/ returns stake)) 0.01))
				  my-list))))

	(sort my-list #'> :key #'fifth)))

(defun do-streak-wins-calc (series &optional (n 10))
  (first-n n (do-streak series #'home-aways #'home-away-win-result #'home-away-odds)))
(defun do-streak-defeats-calc (series &optional (n 10))
  (first-n n (do-streak series #'home-aways #'home-away-lost-result #'home-away-lost-odds)))
(defun do-streak-draws-calc (series &optional (n 10))
  (first-n n (do-streak series #'home-aways #'home-away-draw-result #'series-draw-odds)))
(defun do-streak-overs-calc (series &optional (n 10))
  (first-n n (do-streak series #'home-aways #'series-overs #'series-over-odds)))
(defun do-streak-unders-calc (series &optional (n 10))
  (first-n n (do-streak series #'home-aways #'series-unders #'series-under-odds)))

(defun do-team-streak (s team games-fn result-fn odds-fn)
  "Returns a list of all teams sorted by their returns using series S given the 
   games returned by GAMES-FN which match results from RESULT-FN at odds ODDS-FN"
  
  (let ((stake 0)
		(returns 0)
		(signal-result 0))
	(series-reset s)

	(dolist (game (funcall games-fn team))
	  (let ((current (series-current s)))
		(if (> signal-result 0)
			(+= stake current))
		(cond ((funcall result-fn team game)
			   (if (> signal-result 0)
				   (progn
					 (+= returns (* current (funcall odds-fn team game)))
					 (series-update s "W")
					 (format t "~%W : ~a v ~a Stake : ~a Return : ~a" (home-team game) (away-team game) stake returns))
				   (format t "~%X : ~a v ~a" (home-team game) (away-team game)))
			   (incf signal-result)
			   (if (> signal-result 3)
				   (setf signal-result 0))			   )

			  (t (series-update s "R")
				 (if (> signal-result 0)
					 (format t "~%L : ~a v ~a Stake : ~a Return : ~a" (home-team game) (away-team game) stake returns)
					 (format t "~%X : ~a v ~a" (home-team game) (away-team game)))
	 
				 (setf signal-result 0)))))))

(defun do-team-streak-wins-calc (team series)
  (do-team-streak series team #'home-aways #'home-away-win-result #'home-away-odds))
(defun do-team-streak-defeats-calc (team series)
  (do-team-streak series team #'home-aways #'home-away-lost-result #'home-away-lost-odds))
(defun do-team-streak-draws-calc (team series)
  (do-team-streak series team #'home-aways #'home-away-draw-result #'series-draw-odds))
(defun do-team-streak-overs-calc (team series)
  (do-team-streak series team #'home-aways #'series-overs #'series-over-odds))
(defun do-team-streak-unders-calc (team series)
  (do-team-streak series team #'home-aways #'series-unders #'series-under-odds))

;; ===========
;; 11-13/01/21


(defparameter streak-teams
  '(("Maidenhead" Wins)
	("Stoke" Wins)
	("Man City" Overs) ;; signal for under 130121
	("Alloa" Wins)
	("Sheffield United" Defeats)
	("Wigan" Wins)
	("Annan Athletic" Defeats)
	("Cove Rangers" Defeats)))

(defun make-signal-funcs-ht ()
  (let ((ht (make-hash-table)))
	(setf (gethash 'Wins ht) #'is-win)
	(setf (gethash 'Draws ht) #'is-draw)
	(setf (gethash 'Defeats ht) #'is-defeat)
	(setf (gethash 'Overs ht) #'series-overs)
	(setf (gethash 'Unders ht) #'series-unders)
	ht))

(defun check-for-signal-result (team fn)
  (let ((my-list (last-six team 2)))
	(and (funcall fn team (second my-list))          ;; most recent game
		 (not (funcall fn team (first my-list))))))  ;; previous game

(defun get-signal-results ()
  (let ((signal-funcs-ht (make-signal-funcs-ht))
		(signal-list nil))
	(mapcar #'(lambda (streak-pair)
				(destructuring-bind (team result) streak-pair
				  (if (check-for-signal-result team (gethash result signal-funcs-ht))
					  (push team signal-list))))
			streak-teams)
	signal-list))

(defvar *streak-funcs*
  `(("Wins" ,#'do-streak-wins-calc)
	("Draws" ,#'do-streak-draws-calc)
	("Defeats" ,#'do-streak-defeats-calc)
	("Overs" ,#'do-streak-overs-calc)
	("Unders" ,#'do-streak-unders-calc)))

(defun write-streaks (series funcs filename n)
  (with-open-file (stream filename
						  :direction :output
						  :if-exists :supersede)
	(dolist (series-pair funcs)
	  (destructuring-bind (streak-result streak-fn) series-pair
		(format stream "~a~%" streak-result)
		(dolist (my-list (funcall streak-fn series n))
		  (format stream "~{~a~^,~}~%" my-list)))
	  (format stream "~%"))))

(defun export-streaks (series &optional (n 50))
  (write-streaks series *streak-funcs* "c:/mine/lisp/data/streaks.csv" n)
  t)

