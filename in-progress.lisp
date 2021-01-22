;; ===========
;; 11-13/01/21


(defparameter streak-teams
  '(("Man City" Unders)
	("Sheffield United" Defeats)
	("Shrewsbury" Wins)
	("West Ham" Wins)
	("Eastleigh" Draws)
	("Cove Rangers" Defeats)
	("Airdrie Utd" Defeats)
	("Annan Athletic" Defeats)
	("Sheffield Weds" Defeats)
	("Dover Athletic" Defeats)
	("Derby" Defeats)
	("Queens Park" Overs)
	("Clyde" Overs)
	("Peterhead" Unders)
	("Fulham" Unders)))

(defun make-signal-funcs-ht ()
  (let ((ht (make-hash-table)))
	(setf (gethash 'Wins ht) #'is-win)
	(setf (gethash 'Draws ht) #'is-draw)
	(setf (gethash 'Defeats ht) #'is-defeat)
	(setf (gethash 'Overs ht) #'series-overs)
	(setf (gethash 'Unders ht) #'series-unders)
	ht))

(defun check-for-signal-result (team fn)
  (let ((game-list (reverse (last-n (home-aways team) 5))))
	(cond ((funcall fn team (first game-list))
		   (or (not (funcall fn team (second game-list))) ;; beginning of new streak
			   (and (funcall fn team (third game-list))   ;; continuation of previous streak
					(funcall fn team (fourth game-list))
					(funcall fn team (fifth game-list)))))
		  (t nil))))

(defun get-signal-results ()
  (let ((signal-funcs-ht (make-signal-funcs-ht))
		(signal-list nil))
	(mapcar #'(lambda (streak-pair)
				(destructuring-bind (team result) streak-pair
				  (when (check-for-signal-result team (gethash result signal-funcs-ht))
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

#|
(defun check-for-signal-result-old (team fn)
  (let ((recent-game-list (last-six team 2)))
	(and (funcall fn team (second recent-game-list))          ;; most recent game
		 (not (funcall fn team (first recent-game-list))))))  ;; previous game
|#
