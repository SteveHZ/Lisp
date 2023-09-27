;;(defpackage :football (:use :common-lisp :cl-csv))
;;(eval-when (:compile-toplevel :load-toplevel :execute)  (require 'iterate))
(defpackage :football (:use :common-lisp :cl-csv :iterate))
(defun testing (n)  (iter (for i from 1 to n) (print i)))

;;(load "repl.lisp")
;;  (fdb league)
;;  (game-repl "Football > ")

(defun fdb-welsh ()
  (setf *db* (import-csv "c:/mine/lisp/data/welsh.csv")))

(defun fdb (league)
  (setf *db* (import-csv (format nil "c:/mine/lisp/data/~a.csv" league))))

(defmacro get-key (team)  `(gethash ,team ht))

(defun get-games-func (fn team)
  (remove-if-not
   #'(lambda (game) (funcall fn team game))
   *db*))

(defun load-fixtures ()
  (setf *fixtures* (import-csv "c:/mine/lisp/data/fixtures.csv")))

(defun fhome (game) (third game))
(defun faway (game) (fourth game))

(defun show-fixtures ()
  (dolist (game *fixtures*)
    (format t "~%~a v ~a" (fhome game) (faway game))))

(defparameter ht (make-hash-table :test #'equal))

(defun make-team (teamname)
  (list :team teamname :home 0 :away 0 :av-home 0 :av-away 0))

(defun add-team (team) (push team *expect*))

(defun build-expect (league)
  (dolist (team (get-teams league))
    (add-team (make-team team))))

; ***************************************************************
(defun make-ht-team ()
  (list :home-for 0 :home-ag 0 :away-for 0 :away-ag 0))

(defun make-ht-team2 ()
  (defvar ht2 (make-hash-table :test #'equal)
    (dolist (property '(home-for home-ag away-for away-ag))
      (setf (gethash property ht2) "0"))))

(defun xbuild-ht-expect (league)
  (dolist (team (get-teams league))
    (setf (gethash team ht) (make-ht-team))))

(defun xget-home-for (team)
  (getf (gethash team ht) :home-for))

(defun xget-home-ag (team)
  (getf (gethash team ht) :home-ag))

(defun xget-away-for (team)
  (getf (gethash team ht) :away-for))

(defun xget-away-ag (team)
  (getf (gethash team ht) :away-ag))

(defun xset-home-for (team val)
  (let ((previous (getf (gethash team ht) :home-for)))
    (setf (getf (gethash team ht) :home-for) (+ previous val))))

(defun xset-home-ag (team val)
  (let ((previous (getf (gethash team ht) :home-ag)))
    (setf (getf (gethash team ht) :home-ag) (+ previous val))))

(defun xset-away-for (team val)
  (let ((previous (getf (gethash team ht) :away-for)))
    (setf (getf (gethash team ht) :away-for) (+ previous val))))

(defun xset-away-ag (team val)
  (let ((previous (getf (gethash team ht) :away-ag)))
    (setf (getf (gethash team ht) :away-ag) (+ previous val))))

; ***************************************************************

(defstruct stats
  (home-for 0)
  (home-ag 0)
  (away-for 0)
  (away-ag 0))

(defun show-hash (hash)
  (maphash #'(lambda (k v)
               (format t "~%~a => ~a" k v)) hash))

(defun get-team (team)
  (gethash team ht))

(defun get-home-for (team)
  (stats-home-for (get-team team)))

(defun get-home-ag (team)
  (stats-home-ag (get-team team)))

(defun get-away-for (team)
  (stats-away-for (get-team team)))

(defun get-away-ag (team)
  (stats-away-ag (get-team team)))

(defun set-home-for (team val)
  (let ((previous (stats-home-for (get-team team))))
    (setf (stats-home-for (get-team team)) (+ previous val))))

(defun set-home-ag (team val)
  (let ((previous (stats-home-ag (get-team team))))
    (setf (stats-home-ag (get-team team)) (+ previous val))))

(defun set-away-for (team val)
  (let ((previous (stats-away-for (get-team team))))
    (setf (stats-away-for (get-team team)) (+ previous val))))

(defun set-away-ag (team val)
  (let ((previous (stats-away-ag (get-team team))))
    (setf (stats-away-ag (get-team team)) (+ previous val))))

(defun do-goals (league)
  (build-ht-expect league)
  (dolist (game *db*)
    (set-home-for (home-team game) (parse-integer (home-score game)))
    (set-home-ag  (home-team game) (parse-integer (away-score game)))
    (set-away-for (away-team game) (parse-integer (away-score game)))
    (set-away-ag  (away-team game) (parse-integer (home-score game)))))

(defmacro get-value (team property)
  `(,property (gethash ,team ,ht)))

(defun mget-home-for (team)  (get-value team stats-home-for))
(defun mget-home-ag  (team)  (get-value team stats-home-ag))
(defun mget-away-for (team)  (get-value team stats-away-for))
(defun mget-away-ag  (team)  (get-value team stats-away-ag))

(defmacro set-value (team val property)
  `(let ((previous (,property (get-team ,team))))
     (setf (,property (get-team ,team)) (+ previous ,val))))

(defun mset-home-for (team val)  (set-value team val stats-home-for))
(defun mset-home-ag  (team val)  (set-value team val stats-home-ag))
(defun mset-away-for (team val)  (set-value team val stats-away-for))
(defun mset-away-ag  (team val)  (set-value team val stats-away-ag))

(defun xget-teams (league)   (second (assoc league *teams*)))

(defun xdo-goals (&optional (league 'e0))
  (build-expect league)
  (dolist (game *db*)
    (let ((home (home-team game))
          (away (away-team game)))
      (set-home-for home (parse-integer (home-score game)))
      (set-home-ag  home (parse-integer (away-score game)))
      (set-away-for away (parse-integer (away-score game)))
      (set-away-ag  away (parse-integer (home-score game)))
      (update-game-count home away))))

(defun xget-league (team)
  (setf *db* (import-csv (format nil "c:/mine/lisp/data/~a.csv" (find-league team)))))

(defun poisson (expect, score)
  (/ (* (expt expect score)
        (expt e (- expect 1)))
     (factorial score)))

(defun do-stats (&optional (db *db*))
 (if (null db) (return-from do-stats t))
 (let ((lg-hash (make-hash-table :test #'equal)))
   (build-expect (caar db) lg-hash)
   (dolist (game (cdar db))
     (let ((home (home-team game))
           (away (away-team game)))
       (set-home-for lg-hash home (parse-integer (home-score game)))
       (set-home-ag  lg-hash home (parse-integer (away-score game)))
       (set-away-for lg-hash away (parse-integer (away-score game)))
       (set-away-ag  lg-hash away (parse-integer (home-score game)))
       (update-game-count lg-hash home away)))
   (setf (gethash (caar db) ht-stats) lg-hash))

(defun do-expects ()
 (maphash #'(lambda (lg lg-hash)
              (declare (ignore lg))
              (maphash #'(lambda (team stats)
                          (declare (ignore stats))
                           (let ((home-games (get-home-games lg-hash team))
                                 (away-games (get-away-games lg-hash team)))
                             (set-av-home-for lg-hash team (/ (get-home-for lg-hash team) home-games))
                             (set-av-home-ag  lg-hash team (/ (get-home-ag  lg-hash team) home-games))
                             (set-av-away-for lg-hash team (/ (get-away-for lg-hash team) away-games))
                             (set-av-away-ag  lg-hash team (/ (get-away-ag  lg-hash team) away-games))))
                       lg-hash))
          ht-stats))

(defun do-expects ()
  (load-fixtures)
  (if (null *fixtures*) (return-from do-expects nil))
  (dolist (game *fixtures*)
    (let ((temp (make-game))
          (home-team (fhome game))
          (away-team (faway game))
          (league (fleague game)))
      (setf (game-league temp) league)
      (setf (game-home-team temp) home-team)
      (setf (game-away-team temp) away-team)
      ;; (get-value (gethash (fleague game) ht-stats) (fhome game) stats-av-home-for)
      ;; (get-av-home-for (gethash (fleague game) ht-stats) (fhome game))
      ;; (get-av-home-for (get-stats fleague) (fhome game))
      ;; (get-av-home-for-stats (fhome game))
      ;; (get-av-home-for-stats home-team league)
      ;; (ht-stats (av-home-for home-team league) ?? would just ht-stats work ??

      (setf (game-home-goals temp) (* (ht-stats stats-av-home-for home-team league)
                                      (get-value (faway game) (gethash (fleague game) ht-stats) stats-av-away-ag)
                                      (/ (get-value (fleague game) ht-league-stats league-stats-home-goals)
                                       (get-value (fleague game) ht-league-stats league-stats-games))))

      (setf (game-away-goals temp) (* (get-value (faway game) (gethash (fleague game) ht-stats)  stats-av-away-for)
                                      (get-value (fhome game) (gethash (fleague game) ht-stats)  stats-av-home-ag)
                                      (/ (get-value (fleague game) ht-league-stats league-stats-away-goals)
                                         (get-value (fleague game) ht-league-stats league-stats-games))))
      (setf (game-goal-diff temp) (- (game-home-goals temp) (game-away-goals temp)))
      (print temp))))

(defmacro set-value (team hash value property)
  `(setf (,property (gethash ,team ,hash)) ,value)

(defmacro incr-value (team hash value property)
  `(let ((previous (,property (gethash ,team ,hash))))
    (setf (,property (gethash ,team ,hash)) (+ previous ,value)))

(defmacro ht-stats (property team league)
  `(get-value ,property ,team (gethash ,league ht-stats)))

(defmacro ht-league-stats (property league)
  `(get-value ,property ,league ht-league-stats))

(defun do-expects ()
  "Calculates goal expectancy values for each fixture"
  (load-fixtures)
  (if (null *fixtures*) (return-from do-expects nil))
  (setf *expects* nil)
  (dolist (game *fixtures*)
    (let ((temp (make-game))
          (home-team (fhome game))
          (away-team (faway game))
          (league (fleague game)))
      (setf (game-league temp) league)
      (setf (game-home-team temp) home-team)
      (setf (game-away-team temp) away-team)

      (setf (game-home-goals temp) (* (get-expect-home-for home-team league)
                                      (get-expect-away-ag away-team league)
                                      (get-league-stats-av-home-goals league)))

      (setf (game-away-goals temp) (* (get-expect-away-for away-team league)
                                      (get-expect-home-ag home-team league)
                                      (get-league-stats-av-away-goals league)))

      (setf (game-goal-diff temp) (- (game-home-goals temp)
                                     (game-away-goals temp)))
      (push temp *expects*))))

;; Convert from 2D list ((...)) to 1D lists
(defun build-stats (league hash)
  (destructuring-bind (teams) (get-teams league) (list :teams) ; convert to 1d list
    (dolist (team teams)
      (setf (gethash team hash) (make-stats))))
  (setf (gethash league ht-league-stats) (make-league-stats)))

(defun do-stats (&optional (db *db*))
  "Calculates stats for all teams"
  (dolist (league db)
    (let ((league-hash (make-hash-table :test #'equal))
          (league-name (car league)))
      (build-stats league-name league-hash)

      (dolist (game (cdr league))
        (update-team-stats league-hash game)
        (update-league-stats league-name game))
      (set-league-averages (league-name league-hash))

      (setf (gethash league-name *ht-stats*) league-hash)
      (set-league-stats-av-home-goals league-name (/ (get-league-stats-home-goals league-name)
                                                     (get-league-stats-games league-name)))
      (set-league-stats-av-away-goals league-name (/ (get-league-stats-away-goals league-name)
                                                     (get-league-stats-games league-name))))))




#|
(defmacro get-value2 (property team league &optional (hash *ht-stats*))
  `(let ((my-hash (gethash ,league ,hash)))
	 `(,property (gethash ,team ,my-hash)))
;;  `(,property (gethash ,team (gethash ,league ,hash)))
)
(defun get-home-forx (team league)
  (get-value2 stats-home-for team league))
|#
(defmacro get-value2 (property team league hash)
  `(,property (gethash ,team (gethash ,league ,hash)))
  )
(defun get-home-for (team league &optional (hash *ht-stats*))
  (get-value2 stats-home-for team league hash))

#|
;;(defun get-home-for (team league)  (get-value stats-home-for team (gethash league *ht-stats*)))
(defun get-home-for (team league)
  (get-value2 stats-home-for team league))
(defun get-home-ag (team league)
  (get-value stats-home-ag team (gethash league *ht-stats*)))
(defun get-away-for (team league)
  (get-value stats-away-for team (gethash league *ht-stats*)))
(defun get-away-ag (team league)
  (get-value stats-away-ag team (gethash league *ht-stats*)))

(defun get-av-home-for (team league)
  (get-value stats-av-home-for team (gethash league *ht-stats*)))
(defun get-av-home-ag (team league)
  (get-value stats-av-home-ag team (gethash league *ht-stats*)))
(defun get-av-away-for (team league)
  (get-value stats-av-away-for team (gethash league *ht-stats*)))
(defun get-av-away-ag (team league)
  (get-value stats-av-away-ag team (gethash league *ht-stats*)))

(defun get-expect-home-for (team league)
  (get-value stats-expect-home-for team (gethash league *ht-stats*)))
(defun get-expect-home-ag (team league)
  (get-value stats-expect-home-ag team (gethash league *ht-stats*)))
(defun get-expect-away-for (team league)
  (get-value stats-expect-away-for team (gethash league *ht-stats*)))
(defun get-expect-away-ag (team league)
  (get-value stats-expect-away-ag team (gethash league *ht-stats*)))
|#
(defun get-home-ag (team league)
  (get-value2 stats-home-ag team league))
(defun get-away-for (team league)
  (get-value2 stats-away-for team league))
(defun get-away-ag (team league)
  (get-value2 stats-away-ag team league))

(defun get-av-home-for (team league)
  (get-value2 stats-av-home-for team league))
(defun get-av-home-ag (team league)
  (get-value2 stats-av-home-ag team league))
(defun get-av-away-for (team league)
  (get-value2 stats-av-away-for team league))
(defun get-av-away-ag (team league)
  (get-value2 stats-av-away-ag team league))

(defun get-expect-home-for (team league)
  (get-value2 stats-expect-home-for team league))
(defun get-expect-home-ag (team league)
  (get-value2 stats-expect-home-ag team league))
(defun get-expect-away-for (team league)
  (get-value2 stats-expect-away-for team league))
(defun get-expect-away-ag (team league)
  (get-value2 stats-expect-away-ag team league))

(defun unbeaten-homes ()
  "Show all teams on an unbeaten home run"
  (labels ((inner (games count)
			 (cond ((equal "H" (result (car games)))
					(inner (rest games) (1+ count)))
				   (t count))))

	(let ((unbeaten nil))
	  (dolist (league '("e0" "e1")) ;; *leagues*
		(let ((teams (get-teams league))) ;; (car league)
		  (dolist (team teams)
			(let* ((games (reverse (homes team)))
				   (wins (inner games 0)))
			  (if (>= wins 2)
				  (push `(,team ,wins) unbeaten))))))
	  (sort unbeaten #'> :key #'second))))

(defun unbeaten-aways ()
  "Show all teams on an unbeaten away run"
  (labels ((inner (games count)
			 (cond ((equal "A" (result (car games)))
					(inner (rest games) (1+ count)))
				   (t count))))

	(let ((unbeaten nil))
	  (dolist (league '("e0" "e1")) ;; *leagues*
		(let ((teams (get-teams league))) ;; (car league)
		  (dolist (team teams)
			(let* ((games (reverse (aways team)))
				   (wins (inner games 0)))
			  (if (> wins 1)
				  (push `(,team ,wins) unbeaten))))))
	  (sort unbeaten #'> :key #'second))))

(defun unbeaten (fn test-func)
  (labels ((inner (games count)
			 (cond ((funcall test-func (car games))
					(inner (rest games) (1+ count)))
				   (t count))))

	(let ((unbeaten nil))
	  (dolist (league '("e0" "e1")) ;; *leagues*
		(let ((teams (get-teams league))) ;; (car league)
		  (dolist (team teams)
			(let* ((games (reverse (funcall fn team)))
				   (wins (inner games 0)))
			  (if (>= wins 2)
				  (push `(,team ,wins) unbeaten))))))
	  (sort unbeaten #'> :key #'second))))

(defun unbeaten-homes ()
  "Show all teams on an unbeaten home run"
  (unbeaten #'homes
			 #'(lambda (game)
				 (equal "H" (result game)))))

(defun unbeaten-aways ()
  "Show all teams on an unbeaten away run"
  (unbeaten #'aways
			 #'(lambda (game)
				 (equal "A" (result game)))))

(defun unbeaten-home-aways2 ()
  "Doesn't work with unbeaten above as needs team to be passed in for home-away-win-result"
  "Show all teams on an unbeaten home and away run"
  (unbeaten #'home-aways
			#'(lambda (game)
				(home-away-win-result team (car games)))))

(defun xdo-series-wins (&optional (s (make-series '(1 2 3 4 5 6) '(5 4 3 4 5 6))))
  (let ((my-list nil))
	(dolist (league '("e0" "e1"))
	  (dolist (team (get-teams league))
		(let ((stake 0)
			  (returns 0))
		  (series-reset s)
		  (dolist (game (home-aways team))
			(let ((current (series-current s)))
			  (cond ((home-away-win-result team game)
					 (+= stake current)
					 (+= returns (* current (home-away-odds team game)))
					 (series-update s "W"))
					(t (+= stake current)
					   (series-update s "L")))))
		  (push `(,team ,stake ,returns
						,(my-round (* 100 (/ returns stake)) 0.01))
				my-list))))
	(sort my-list #'> :key #'fourth)))

(defun xdo-series-defeats (&optional (s (make-series '(1 2 3 4 5 6) '(5 4 3 4 5 6))))
  (let ((my-list nil))
	(dolist (league '("e0" "e1"))
	  (dolist (team (get-teams league))
		(let ((stake 0)
			  (returns 0))
		  (series-reset s)
		  (dolist (game (home-aways team))
			(let ((current (series-current s)))
			  (cond ((home-away-lost-result team game)
					 (+= stake current)
					 (+= returns (* current (home-away-lost-odds team game)))
					 (series-update s "W"))
					(t (+= stake current)
					   (series-update s "L")))))
		  (push `(,team ,stake ,returns
						,(my-round (* 100 (/ returns stake)) 0.01))
				my-list))))
	(sort my-list #'> :key #'fourth)))

(defun unbeaten-home-aways ()
  "Show all teams on an unbeaten home and away run, the only difference between this and the consecutive-games
   macro below is that this needs 'team' to be passed to the inner label to pass to home-away-win-result"
  ;; perhaps write macro below with an optional parameter ?? not sure this will work with the funcall though

  (labels ((inner (team games count)
			 (cond ((null games) count)
				   ((home-away-win-result team (car games))
					(inner team (rest games) (1+ count)))
				   (t count))))

	 (let ((my-list nil))
	   (dolist (league '("e0" "e1")) ;; *leagues*
		 (let ((teams (get-teams league))) ;; (car league)
		   (dolist (team teams)
			 (let* ((games (reverse (home-aways team)))
					(wins (inner team games 0)))
			   (if (>= wins 2)
				   (push `(,team ,wins) my-list))))))
	   (sort my-list #'> :key #'second))))

(defun show-fixtures ()
  "Shows current fixtures file, will return NIL if not loaded"

  (if (null *fixtures*) (return-from show-fixtures nil))
  (dolist (game *fixtures*)
    (format t "~%~a - ~a v ~a" (fleague game) (fhome game) (faway game))))

(defun consecutive-gamesx (fn test-func)
  "Used by unbeaten and consecutive-draw functions below, shows all teams on an unbeaten home and/or away run.
   unbeaten-home-aways is the only function that uses 'team' passed into the 'inner' function,
   all other functions have to work around this"

  (labels ((inner (team games count)
			 (cond ((null games) count)
				   ((funcall test-func team (car games))
					(inner team (rest games) (1+ count)))
			   (t count))))

	(let ((my-list nil))
	  (dolist (league '("e0" "e1")) ;; *leagues*
		(let ((teams (get-teams league))) ;; (car league)
		  (dolist (team teams)
			(let* ((games (reverse (funcall fn team)))
				   (wins (inner team games 0)))
			  (when (>= wins 2)
				(push `(,team ,wins) my-list))))))
	  (sort my-list #'> :key #'second))))

(defun count-seasonx (fn test-fn)
  "Returns a list of all teams sorted by the games returned by FN
   which match the result TEST-FN"
  (labels ((inner (games count)
			 (cond ((null games) count)
				   ((funcall test-fn (car games))
					(inner (rest games) (1+ count)))
				   (t (inner (rest games) count)))))

	(let ((my-list nil)) ;; *leagues*
  	  (dolist (league '("e0" "e1")) ;; (car league)
        		(let ((teams (get-teams league)))
		  (dolist (team teams)
			(let* ((games (funcall fn team))
				   (count (inner games 0)))
			  (push `(,team ,(length games) ,count
							,(my-round (* 100 (/ count (length games))) 0.01))
					my-list)))))
	  (sort my-list #'> :key #'fourth))))

(defun do-seriesx (s games-fn result-fn odds-fn)
  "Returns a list of all teams sorted by the returns using series S given the
   games returned by GAMES-FN which match results from RESULT-FN at odds ODDS-FN"

  (let ((my-list nil))
	(dolist (league '("e0" "e1"))
  	(dolist (team (get-teams league))
  		(let ((stake 0)
			  (returns 0))
		  (series-reset s)
		  (dolist (game (funcall games-fn team))
			(let ((current (series-current s)))
			  (cond ((funcall result-fn team game)
					 (+= stake current)
					 (+= returns (* current (funcall odds-fn team game)))
					 (series-update s "W"))
					(t (+= stake current)
					   (series-update s "L")))))
		  (push `(,team ,stake ,returns
						,(my-round (* 100 (/ returns stake)) 0.01))
  				my-list))))
  	(sort my-list #'> :key #'fourth)))

(defun max-games-no-drawx ()
  (let ((my-list nil))
	(dolist (league '("e0" "e1"))
	  (dolist (team (get-teams league))
		(let ((count 0)
			  (max 0))
  		  (dolist (game (home-aways team))
  			(cond ((equal "D" (result game))
				   (when (> count max)
  					 (setf max count))
			   (setf count 0))
				  (t (incf count))))
		  (when (> count max)
			(setf max count))
		  (push `(,team ,max) my-list))))
    	(sort my-list #'> :key #'second)))

(defmacro with-all-teams2 ((team leagues) &body body)
  (with-gensyms (gteam gleagues)
 	`(let ((,gteam ,team)
   		   (,gleagues ,leagues))
 	   (dolist (league ,gleagues)
  		  (dolist (,gteam (get-teams (car league)))
     			,@body)))))

(if (> date (get-days-in-month month))
	(progn
    (if (and (equal month 2)
  	    (simple-is-leap-year year))
 	    (-= date 29)
    	(-= date (get-days-in-month month)))
    	(incf month)

   	(if (> month 12)
    	(progn
  	   (-= month 12)
	     (incf year)))))

(defun show-av-league-goals ()
  "Show average goals stats for each league"
  (dolist (league *leagues*)
	(format t "~%League : ~a  ~30tHome : ~,2f Away : ~,2f"
			(cadr league)
			(get-league-stats-av-home-goals (car league))
			(get-league-stats-av-away-goals (car league)))))

(defun do-series-all-last-six (series &optional (n 10))
  (let ((my-list nil))
	(mapcar #'(lambda (result result-fn odds-fn)
				(dolist (results (do-series series #'last-six result-fn odds-fn))
				  (push (cons result results) my-list)))
			series-results series-fns series-odds-fns)
	(series-all-table
	 (first-n n (sort my-list #'> :key #'sixth)))))
   
(defmacro say (&body body)  `(say-games ,@body))

;; THIS DOESN'T WORK !!
;; https://stackoverflow.com/questions/59307111/why-is-mapcar-only-using-one-of-the-arguments-returned-from-values-list
(defun export-series (series)
  (export-csv
   (mapcar (lambda (fn)
			 (mapcar #'(lambda (my-list)
						 (list (first my-list) (second my-list) (fifth my-list))
;;						 (princ (format nil "~a, ~a, ~a~%" (first my-list) (second my-list) (fifth my-list)))
;;						 (format nil "~a, ~a, ~a" (first my-list) (second my-list) (fifth my-list))
						 )
					 (funcall fn series 10)))
		   (list
;;               #'do-series-wins-list
;;				 #'do-series-home-wins-list
;;				 #'do-series-away-wins-list
				 #'do-series-draws-list
;;				 #'do-series-home-draws-list
;;				 #'do-series-away-draws-list
				 ))
   "c:/mine/lisp/data/series3.csv"))

(defun is-leap-year (y)
  "Calculates whether Y is a leap year correctly"
  (cond ((= (mod y 100) 0)
         (if (= (mod y 400) 0)
             t nil))
        ((equal (mod y 4) 0) t)
        (t nil)))

;; Original version didn't work correctly as Lisp exports each csv line within quotes.
;; When loaded into a spreadsheet, this showed each quoted line together in a single cell.
;; This version writes out to csv which is then re-worked by perl series.pl to an xlsx file.

(defun export-series (series)
  (export-csv
   (mapcar (lambda (fn)
			 (mapcar #'(lambda (my-list)
						 (format nil "~a, ~a, ~a"
								 (first my-list)
								 (second my-list)
								 (fifth my-list)))
					 (funcall fn series 20)))
		   (list
			  #'do-series-wins-list
			  #'do-series-home-wins-list
			  #'do-series-away-wins-list
			  #'do-series-draws-list
			  #'do-series-home-draws-list
			  #'do-series-away-draws-list))
   "c:/mine/lisp/data/series.csv")
  t)

;;; Write export-series2 accepting list of series to write files for each series
;;; prob call this from another function which will recieve the list
;;; other func (series-export) would do path/filename then pass here
;;; doesn't work because series s1 and s2 (closures) are not the same as (list s1 s2)

(defun export-series (series)
  (with-open-file (stream "c:/mine/lisp/data/series.csv"
						  :direction :output
						  :if-exists :supersede)
	(mapcar #'(lambda (fn)				
				(mapcar #'(lambda (my-list)
							(format stream "~a,~a,~a~%"
									(first my-list)
									(second my-list)
									(fifth my-list)))
						(funcall fn series 20))
				(format stream "~%"))
			(list
			 #'do-series-wins-list
			 #'do-series-home-wins-list
			 #'do-series-away-wins-list
			 #'do-series-draws-list
			 #'do-series-home-draws-list
			 #'do-series-away-draws-list))
	'done))

#||
(defun export-series2 (series-list)
  (let ((filenames '(s1 s2 s3 s4)))
	(do* ((series series-list)
		  (filename filenames))
		 (export-series series filename))))
|#















(defparameter series-list `((,s1 "s1")
							(,s2 "s2")
							(,s3 "s3")
							(,s4 "s4")
							(,s5 "s5")))

(defun export-series (series)
  (with-open-file (stream "c:/mine/lisp/data/series.csv"
						  :direction :output
						  :if-exists :supersede)
	(let ((funcs (list
				  #'do-series-wins-list
				  #'do-series-home-wins-list
				  #'do-series-away-wins-list
				  #'do-series-draws-list
				  #'do-series-home-draws-list
				  #'do-series-away-draws-list)))
	  
	  (dolist (fn funcs 'done)
		(dolist (my-list (funcall fn series 20))
		  (format stream "~a,~a,~a~%"
				  (first my-list)
				  (second my-list)
				  (fifth my-list)))
		(format stream "~%")))))

(defun new-export-series (series filename)
  (with-open-file (stream filename
						  :direction :output
						  :if-exists :supersede)
(print series)
	(let ((funcs (list
				#'do-series-wins-list
				#'do-series-home-wins-list
				#'do-series-away-wins-list
				#'do-series-draws-list
				#'do-series-home-draws-list
				#'do-series-away-draws-list)))
	  
	  (dolist (fn funcs)
		(dolist (my-list (funcall fn series 20))
		  (format stream "~a,~a,~a~%"
				  (first my-list)
				  (second my-list)
				  (fifth my-list)))
		(format stream "~%")))))

(defun export-series2 ()
  (dolist (series series-list 'done)
	(let ((filename (format nil "c:/mine/lisp/data/series ~a.csv" (cadr series))))
	  (new-export-series (car series) filename))))

(defun do-odds ()
  "Calculates odds for each game from goal expectancy values"
  (dolist (game *expects*)
    (let ((ds (make-instance 'my-odds :size 10)))
      (calc-game ds (game-home-goals game) (game-away-goals game))
	  (multiple-value-bind (home draw away) (vget-odds ds)
      (setf (game-home-odds game) home)
      (setf (game-draw-odds game) draw)
      (setf (game-away-odds game) away))))
#|
      (let ((odds-list (get-odds ds)))
        (setf (game-home-odds game) (first odds-list))
        (setf (game-draw-odds game) (second odds-list))
        (setf (game-away-odds game) (third odds-list)))
|#

  (format t "~%")
  (print-game-odds (safe-sort *expects*
                     #'< :key #'game-draw-odds)))

(defmacro lengths (&body body)
  "Return the lengths of a given list of lists
   eg (lengths '((1 2 3) (9 8 7 6) (1 2 3 4 5 6 7 8 9))) returns (3 4 9)"
  `(mapcar #'(lambda (list)
			   (length list))
		   ,@body))
       
;; Not tail-recursive

(defun returns (fn list)
  (labels ((inner (list)
             (cond ((null list) 0)
                   (t (+ (funcall fn (car list))
                         (inner (cdr list)))))))
    (inner list)))

(defun ha-returns (fn team list)
  (labels ((inner (list)
			 (cond ((null list) 0)
				   (t (+ (funcall fn team (car list))
						 (inner (cdr list)))))))
	(inner list)))

(defun do-team-series (team s games-fn result-fn odds-fn)
  "Returns details of a TEAM with series S, using games returned from GAMES-FN
   which match RESULT-FN at odds ODDS-FN"
  
  (let ((stake 0)
		(returns 0))
	(series-reset s)
	(format t "~68tOdds ~83tStake ~89tReturn")

	(dolist (game (funcall games-fn team))
	  (let ((current (series-current s)))
		(cond ((funcall result-fn team game)
			   (+= stake current)
			   (+= returns (* current (funcall odds-fn team game)))
			   (series-update s "W")
			   (format t "~%~a  ~a ~31tv ~a ~55t~a  W  ~5,2f ~5,2f ~5,2f  : ~6,2f ~6,2f"
					   (date game) (home-team game) (away-team game) current
					   (home-odds game) (draw-odds game) (away-odds game)
					   stake returns))
			  (t (+= stake current)
				 (format t "~%~a  ~a ~31tv ~a ~55t~a  L  ~5,2f ~5,2f ~5,2f  : ~6,2f ~6,2f"
						 (date game) (home-team game) (away-team game) current
						 (home-odds game) (draw-odds game) (away-odds game)
						 stake returns)
				 (series-update s "L")))))
	(format t "~%~%Stake  : £~$~%Return : £~$" stake returns)))

(defun do-team-series2 (team s games-fn result-fn odds-fn)
  "Returns details of a TEAM with series S, using games returned from GAMES-FN
   which match RESULT-FN at odds ODDS-FN"
  
  (let ((stake 0)
		(returns 0)
		(my-list nil)
		(result ""))

	(series-reset s)
	(format t "~68tOdds ~83tStake ~89tReturn")

	(dolist (game (funcall games-fn team))
	  (let ((current (series-current s)))
		(cond ((funcall result-fn team game)
			   (+= stake current)
			   (+= returns (* current (funcall odds-fn team game)))
			   (setf result "W"))
			  (t (+= stake current)
				 (setf result "L")))
		(series-update s result)
		(push (list (date game) (home-team game) (away-team game) current result
					(home-odds game) (draw-odds game) (away-odds game)
					stake returns)
			  my-list)))

	(format t "~{~{~%~a  ~a ~31tv ~a ~55t~a  ~a  ~5,2f ~5,2f ~5,2f  : ~6,2f ~6,2f~}~}"
			(reverse my-list))
	(format t "~%~%Stake  : £~,2f~%Return : £~,2f" stake returns)))

(defun do-series-wins-calc (series &optional (n 30))
  (first-n n (do-series series #'home-aways #'home-away-win-result #'home-away-odds)))
(defun do-series-home-wins-calc (series &optional (n 30))
  (first-n n (do-series series #'homes #'home-away-win-result #'home-away-odds)))
(defun do-series-away-wins-calc (series &optional (n 30))
  (first-n n (do-series series #'aways #'home-away-win-result #'home-away-odds)))

(defun do-series-defeats-calc (series &optional (n 30))
  (first-n n (do-series series #'home-aways #'home-away-lost-result #'home-away-lost-odds)))
(defun do-series-home-defeats-calc (series &optional (n 30))
  (first-n n (do-series series #'homes #'home-away-lost-result #'home-away-lost-odds)))
(defun do-series-away-defeats-calc (series &optional (n 30))
  (first-n n (do-series series #'aways #'home-away-lost-result #'home-away-lost-odds)))

(defun do-series-draws-calc (series &optional (n 30))
  (first-n n (do-series series #'home-aways #'home-away-draw-result #'series-draw-odds)))
(defun do-series-home-draws-calc (series &optional (n 30))
  (first-n n (do-series series #'homes #'home-away-draw-result #'series-draw-odds)))
(defun do-series-away-draws-calc (series &optional (n 30))
  (first-n n (do-series series #'aways #'home-away-draw-result #'series-draw-odds)))

(defun do-series-overs-calc (series &optional (n 30))
  (first-n n (do-series series #'home-aways #'series-overs #'series-over-odds)))
(defun do-series-home-overs-calc (series &optional (n 30))
  (first-n n (do-series series #'homes #'series-overs #'series-over-odds)))
(defun do-series-away-overs-calc (series &optional (n 30))
  (first-n n (do-series series #'aways #'series-overs #'series-over-odds)))

(defun do-series-unders-calc (series &optional (n 30))
  (first-n n (do-series series #'home-aways #'series-unders #'series-under-odds)))
(defun do-series-home-unders-calc (series &optional (n 30))
  (first-n n (do-series series #'homes #'series-unders #'series-under-odds)))
(defun do-series-away-unders-calc (series &optional (n 30))
  (first-n n (do-series series #'aways #'series-unders #'series-under-odds)))

(defun do-series-wins (series &optional (n 10))
  (series-table (do-series-wins-calc series n)))
(defun do-series-home-wins (series &optional (n 30))
  (series-table (do-series-home-wins-calc series n)))
(defun do-series-away-wins (series &optional (n 30))
  (series-table (do-series-away-wins-calc series n)))

(defun do-series-defeats (series &optional (n 30))
  (series-table (do-series-defeats-calc series n)))
(defun do-series-home-defeats (series &optional (n 30))
  (series-table (do-series-home-defeats-calc series n)))
(defun do-series-away-defeats (series &optional (n 30))
  (series-table (do-series-away-defeats-calc series n)))

(defun do-series-draws (series &optional (n 30))
  (series-table (do-series-draws-calc series n)))
(defun do-series-home-draws (series &optional (n 30))
  (series-table (do-series-home-draws-calc series n)))
(defun do-series-away-draws (series &optional (n 30))
  (series-table (do-series-away-draws-calc series n)))

(defun do-series-overs (series &optional (n 30))
  (series-table (do-series-overs-calc series n)))
(defun do-series-home-overs (series &optional (n 30))
  (series-table (do-series-home-overs-calc series n)))
(defun do-series-away-overs (series &optional (n 30))
  (series-table (do-series-away-overs-calc series n)))

(defun do-series-unders (series &optional (n 30))
  (series-table (do-series-unders-calc series n)))
(defun do-series-home-unders (series &optional (n 30))
  (series-table (do-series-home-unders-calc series n)))
(defun do-series-away-unders (series &optional (n 30))
  (series-table (do-series-away-unders-calc series n)))


;; Second version of do-series-wina etc
;; More functional but caused problems with export-series

(defun do-series-wins-calc (series games-fn n)
  (first-n n (do-series series games-fn #'home-away-win-result #'home-away-odds)))
(defun do-series-defeats-calc (series games-fn n)
  (first-n n (do-series series games-fn #'home-away-lost-result #'home-away-lost-odds)))
(defun do-series-draws-calc (series games-fn n)
  (first-n n (do-series series games-fn #'home-away-draw-result #'series-draw-odds)))
(defun do-series-overs-calc (series games-fn n)
  (first-n n (do-series series games-fn #'series-overs #'series-over-odds)))
(defun do-series-unders-calc (series games-fn n)
  (first-n n (do-series series games-fn #'series-unders #'series-under-odds)))

(defun do-series-home-wins-calc (series n)
  (first-n n (do-series series #'homes #'home-away-win-result #'home-away-odds)))
(defun do-series-away-wins-calc (series n)
  (first-n n (do-series series #'aways #'home-away-win-result #'home-away-odds)))
(defun do-series-home-defeats-calc (series n)
  (first-n n (do-series series #'homes #'home-away-lost-result #'home-away-lost-odds)))
(defun do-series-away-defeats-calc (series n)
  (first-n n (do-series series #'aways #'home-away-lost-result #'home-away-lost-odds)))
(defun do-series-home-draws-calc (series n)
  (first-n n (do-series series #'homes #'home-away-draw-result #'series-draw-odds)))
(defun do-series-away-draws-calc (series n)
  (first-n n (do-series series #'aways #'home-away-draw-result #'series-draw-odds)))

(defun count-league-draws ()
  "Find league with highest average of draws - favs/10 draws strategy"
  (dolist (league *leagues*)
	(let ((games (get-league (car league)))
		  (count 0)
		  (total-games 0))
	  (dolist (game games)
		(incf total-games)
		(if (equal (result game) "D")
			(incf count)))
	  (format t "~%~a : Draws : ~3d Games : ~3d Percent : ~5,2f%"
			  (string-upcase (car league)) count total-games (calc-percent total-games count)))))

(defun count-league-draws2 ()
  (mapcar #'(lambda (league)
			  (let ((count 0)
					(total-games 0))
				;; do this as a label ?? -> pass league as param then count total-games as locals ??
				(mapcar #'(lambda (game)
							(incf total-games)
							(if (equal (result game) "D")
								(incf count)))
						(get-league (car league)))
				(format t "~%~a : Draws : ~3d Games : ~3d Percent : ~5,2f%"
						(string-upcase (car league)) count total-games (calc-percent total-games count))))
		  *leagues*))

(defun count-league-draws3 ()
  (labels ((do-league (league)
			 (let ((count 0)
				   (total-games 0))
			   (mapcar #'(lambda (game)
						   (incf total-games)
						   (if (equal (result game) "D")
							   (incf count)))
					   (get-league (car league)))
			   (format t "~%~a : Draws : ~3d Games : ~3d Percent : ~5,2f%"
					   (string-upcase (car league)) count total-games (calc-percent total-games count)))))

	(mapcar #'(lambda (league)
				(do-league league))
			*leagues*)))

(defun count-league-draws4 ()
  (labels ((do-league (league)
			 (let ((count 0)
				   (total-games 0))
			   (dolist (game (get-league (car league)))
				 (incf total-games)
				 (if (equal (result game) "D")
					 (incf count)))
			   (format t "~%~a : Draws : ~3d Games : ~3d Percent : ~5,2f%"
					   (string-upcase (car league)) count total-games (calc-percent total-games count)))))

	(mapcar #'(lambda (league)
				(do-league league))
			*leagues*)))

(defun count-league-draws ()
  "Find league with highest average of draws - favs/10 draws strategy"
  (dolist (league *leagues*)
	(let ((games (get-league (car league)))
		  (count 0)
		  (total-games 0))
	  (dolist (game games)
		(incf total-games)
		(if (equal (result game) "D")
			(incf count)))
	  (format t "~%~a : Draws : ~3d Games : ~3d Percent : ~5,2f%"
			  (string-upcase (car league)) count total-games (calc-percent total-games count)))))

(defun count-league-overs ()
  "Find league with highest average of over 2.5s"
  (dolist (league *leagues*)
	(let ((games (get-league (car league)))
		  (count 0)
		  (total-games 0))
	  (dolist (game games)
		(incf total-games)
		(if (> (+ (home-score game)
				  (away-score game))
			   2)
			(incf count)))
	  (format t "~%~a : Overs : ~3d Games : ~3d Percent : ~5,2f%"
			  (string-upcase (car league)) count total-games (calc-percent total-games count)))))

(defun sort-btts-v1 ()
  (sort (let ((my-list nil))
		  (dolist (league *uk-leagues*)
			(mapcar #'(lambda (team)
						(push (list team (btts-percent team)) my-list))
					(get-teams (car league))))
		  my-list)
		#'> :key #'second))
    
(defparameter series-funcs
  (list #'do-series-wins
		#'do-series-home-wins-calc
		#'do-series-away-wins-calc
		#'do-series-draws
		#'do-series-home-draws-calc
		#'do-series-away-draws-calc
		#'do-series-defeats
		#'do-series-home-defeats-calc
		#'do-series-away-defeats-calc))
    
(defun show-nested-hash (hash)
  "Prints out stats for the given nested HASH, such as *ht-stats*"
  (maphash #'(lambda (key value)
               (format t "~%~%League : ~a" (cadr (assoc key *leagues*)))
               (show-hash value))
           hash))

(defun say (&optional (games *db*))
  (mapcar #'(lambda (game)
			  (say-game game))
            games))

(defun say-odds (&optional (games *db*))
  (mapcar #'(lambda (game)
			  (say-game-odds game))
          games))

(defun get-last-six (fn team ngames)
  (let* ((my-data (funcall fn team))
		 (len (length my-data))
		 (start-elem (if (>= ngames len)
						 0 (- len ngames))))
	(nthcdr start-elem my-data)))
  
  
  

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

(defun do-team-series (team s games-fn result-fn odds-fn)
  (format t "~68tOdds ~83tStake ~89tReturn")

  (multiple-value-bind (my-list stake returns)
	  (do-team-series-calc team s games-fn result-fn odds-fn)

	(format t "~{~{~%~a  ~a ~30t v ~a ~55t~a  ~a  ~5,2f ~5,2f ~5,2f  : ~6,2f ~6,2f~}~}" my-list)
	(format t "~%~%Stake  : £~,2f~%Return : £~,2f~%Percentage : ~,2f%" stake returns (* (/ returns stake) 100))))

(defun do-team-series-wins (team series)
  (do-team-series team series #'home-aways #'home-away-win-result #'home-away-odds))
(defun do-team-series-home-wins (team series)
  (do-team-series team series #'homes #'home-away-win-result #'home-away-odds))
(defun do-team-series-away-wins (team series)
  (do-team-series team series #'aways #'home-away-win-result #'home-away-odds))

(defun do-team-series-defeats (team series)
  (do-team-series team series #'home-aways #'home-away-lost-result #'home-away-lost-odds))
(defun do-team-series-home-defeats (team series)
  (do-team-series team series #'homes #'home-away-lost-result #'home-away-lost-odds))
(defun do-team-series-away-defeats (team series)
  (do-team-series team series #'aways #'home-away-lost-result #'home-away-lost-odds))

(defun do-team-series-draws (team series)
  (do-team-series team series #'home-aways #'home-away-draw-result #'series-draw-odds))
(defun do-team-series-home-draws (team series)
  (do-team-series team series #'homes #'home-away-draw-result #'series-draw-odds))
(defun do-team-series-away-draws (team series)
  (do-team-series team series #'aways #'home-away-draw-result #'series-draw-odds))

(defun do-team-series-overs (team series)
  (do-team-series team series #'home-aways #'series-overs #'series-over-odds))
(defun do-team-series-home-overs (team series)
  (do-team-series team series #'homes #'series-overs #'series-over-odds))
(defun do-team-series-away-overs (team series)
  (do-team-series team series #'aways #'series-overs #'series-over-odds))

(defun do-team-series-unders (team series)
  (do-team-series team series #'home-aways #'series-unders #'series-under-odds))
(defun do-team-series-home-unders (team series)
  (do-team-series team series #'homes #'series-unders #'series-under-odds))
(defun do-team-series-away-unders (team series)
  (do-team-series team series #'aways #'series-unders #'series-under-odds))

(defun get-league (csv-league &optional (db *db*))
  "Returns a list of all games for given CSV-LEAGUE"
  (cond ((null db) nil)
        ((string-equal csv-league (caar db))
         (cdar db))
        (t (get-league csv-league (rest db)))))
		
(defun get-last-six (fn team ngames)
  (let* ((my-data (funcall fn team))
		 (len (length my-data))
		 (start-elem (if (>= ngames len)
						 0 (- len ngames))))
	(nthcdr start-elem my-data)))
	


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
		
		(if (> stake 0)
			(push (list (string-upcase (csv-filename league))
						team
						stake
						(format nil "~6,2f" returns)
						(my-round (* 100 (/ returns stake)) 0.01))
				  my-list))))

	(sort my-list #'> :key #'fifth)))

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

(defun do-team-series-calc (team s games-list result-fn odds-fn)
  "Returns details of a TEAM with series S, using games in GAMES-LIST
   which match RESULT-FN at odds ODDS-FN"
  
  (let ((my-list nil)
		(stake 0)
		(returns 0)
		(result ""))

	(series-reset s)
	(dolist (game games-list)
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

(defun do-team-series (team s games-fn result-fn odds-fn)
  (format t "~68tOdds ~83tStake ~89tReturn")

  (multiple-value-bind (my-list stake returns)
	  (do-team-series-calc team s games-fn result-fn odds-fn)

	(format t "~{~{~%~a  ~a ~30t v ~a ~55t~a  ~a  ~5,2f ~5,2f ~5,2f  : ~6,2f ~6,2f~}~}" my-list)
	(format t "~%~%Stake  : £~,2f~%Return : £~,2f~%Percentage : ~,2f%" stake returns (* (/ returns stake) 100))))


(defun do-team-series-wins (team series)
  (do-team-series team series (home-aways team) #'home-away-win-result #'home-away-odds))
(defun do-team-series-home-wins (team series)
  (do-team-series team series (homes team) #'home-away-win-result #'home-away-odds))
(defun do-team-series-away-wins (team series)
  (do-team-series team series (aways team) #'home-away-win-result #'home-away-odds))

(defun do-team-series-defeats (team series)
  (do-team-series team series (home-aways team) #'home-away-lost-result #'home-away-lost-odds))
(defun do-team-series-home-defeats (team series)
  (do-team-series team series (homes team) #'home-away-lost-result #'home-away-lost-odds))
(defun do-team-series-away-defeats (team series)
  (do-team-series team series (aways team) #'home-away-lost-result #'home-away-lost-odds))

(defun do-team-series-draws (team series)
  (do-team-series team series (home-aways team) #'home-away-draw-result #'series-draw-odds))
(defun do-team-series-home-draws (team series)
  (do-team-series team series (homes team) #'home-away-draw-result #'series-draw-odds))
(defun do-team-series-away-draws (team series)
  (do-team-series team series (aways team) #'home-away-draw-result #'series-draw-odds))

(defun do-team-series-overs (team series)
  (do-team-series team series (home-aways team) #'series-overs #'series-over-odds))
(defun do-team-series-home-overs (team series)
  (do-team-series team series (homes team) #'series-overs #'series-over-odds))
(defun do-team-series-away-overs (team series)
  (do-team-series team series (aways team) #'series-overs #'series-over-odds))

(defun do-team-series-unders (team series)
  (do-team-series team series (home-aways team) #'series-unders #'series-under-odds))
(defun do-team-series-home-unders (team series)
  (do-team-series team series (homes team) #'series-unders #'series-under-odds))
(defun do-team-series-away-unders (team series)
  (do-team-series team series (aways team) #'series-unders #'series-under-odds))

(defun do-team-series-wins-last-six (team series &optional (n 6))
  (do-team-series team series (last-six team n) #'home-away-win-result #'home-away-odds))
(defun do-team-series-defeats-last-six (team series &optional (n 6))
  (do-team-series team series (last-six team n) #'home-away-lost-result #'home-away-lost-odds))



;; RETURNS DSL

; **************************************************************
;;
;;  DSL Returns
;;

(defun returns (fn my-list)
  (labels ((inner (inner-list acc)
			 (if (null inner-list) acc
				 (inner (cdr inner-list)
						(+ acc (funcall fn (car inner-list)))))))
    (inner my-list 0)))

(defun ha-returns (fn team my-list)
  (labels ((inner (inner-list acc)
			 (if (null inner-list) acc
				 (inner (cdr inner-list)
						(+ acc (funcall fn team (car inner-list)))))))
	(inner my-list 0)))

;; Returns per team

(defun home-returns (team)
  (returns #'home-odds (home-wins team)))
(defun away-returns (team)
  (returns #'away-odds (away-wins team)))

(defun home-percentage-return (team)
  (/ (home-returns team)
     (total-home-games team)))
(defun away-percentage-return (team)
  (/ (away-returns team)
     (total-away-games team)))

(defun home-loss-returns (team)
  (returns #'away-odds (home-defeats team)))
(defun away-loss-returns (team)
  (returns #'home-odds (away-defeats team)))

(defun home-loss-percentage-return (team)
  (/ (home-loss-returns team)
     (total-home-games team)))
(defun away-loss-percentage-return (team)
  (/ (away-loss-returns team)
     (total-away-games team)))

(defun home-away-returns (team)
  (+ (home-returns team)
     (away-returns team)))
(defun home-away-percentage-return (team)
  (/ (home-away-returns team)
     (total-games team)))

(defun home-away-loss-returns (team)
  (+ (home-loss-returns team)
     (away-loss-returns team)))
(defun home-away-loss-percentage-return (team)
  (/ (home-away-loss-returns team)
     (total-games team)))

(defun draw-returns (team)
  (returns #'draw-odds (draws team)))
(defun home-draw-returns (team)
  (returns #'draw-odds (home-draws team)))
(defun away-draw-returns (team)
  (returns #'draw-odds (away-draws team)))

(defun draw-percentage-return (team)
  (/ (draw-returns team)
     (total-games team)))
(defun home-draw-percentage-return (team)
  (/ (home-draw-returns team)
     (total-home-games team)))
(defun away-draw-percentage-return (team)
  (/ (away-draw-returns team)
     (total-away-games team)))

(defun home-away-over-returns (team)
  (returns #'over-odds (home-away-overs team)))
(defun home-over-returns (team)
  (returns #'over-odds (home-overs team)))
(defun away-over-returns (team)
  (returns #'over-odds (away-overs team)))

(defun over-percentage-return (team)
  (/ (home-away-over-returns team)
	 (total-games team)))
(defun home-over-percentage-return (team)
  (/ (home-over-returns team)
	 (total-home-games team)))
(defun away-over-percentage-return (team)
  (/ (away-over-returns team)
	 (total-away-games team)))

(defun home-away-under-returns (team)
  (returns #'under-odds (home-away-unders team)))
(defun home-under-returns (team)
  (returns #'under-odds (home-unders team)))
(defun away-under-returns (team)
  (returns #'under-odds (away-unders team)))

(defun under-percentage-return (team)
  (/ (home-away-under-returns team)
	 (total-games team)))
(defun home-under-percentage-return (team)
  (/ (home-under-returns team)
	 (total-home-games team)))
(defun away-under-percentage-return (team)
  (/ (away-under-returns team)
	 (total-away-games team)))

(defun last-six-homes-return (team)
  (returns #'home-odds (wins-in-last-six-homes team)))
(defun last-six-aways-return (team)
  (returns #'away-odds (wins-in-last-six-aways team)))
(defun last-six-returns (team)
  (ha-returns #'home-away-odds team (wins-in-last-six team)))
(defun last-six-loss-returns (team)
  (ha-returns #'home-away-lost-odds team (defeats-in-last-six team)))
(defun last-six-draws-return (team)
  (returns #'draw-odds (draws-in-last-six team)))

(defun last-six-percentage-return (returns-fn team)
  (let ((len (length (last-six team))))
	(if (zerop len) 0
		(/ (funcall returns-fn team) len))))

(defun last-six-wins-percentage-return (team)
  (last-six-percentage-return #'last-six-returns team))
(defun last-six-home-wins-percentage-return (team)
  (last-six-percentage-return #'last-six-homes-return team))
(defun last-six-away-wins-percentage-return (team)
  (last-six-percentage-return #'last-six-aways-return team))
(defun last-six-draws-percentage-return (team)
  (last-six-percentage-return #'last-six-draws-return team))


;; Returns per league

(defun percents-table (my-list)
  (format-table t my-list
				:column-label '("League" "Team" "Return")
				:column-align '(:left :center :center)))

(defun league-percents (fn csv-league)
  "Calculate (loss) percentage returns for each TEAM in LEAGUE"
  (mapcar #'(lambda (team)
			  (list (string-upcase csv-league) team
					(my-round (funcall fn team) 0.0001)))
          (get-teams csv-league)))

(defun do-league-percents (fn league)
  (percents-table
   (safe-sort (league-percents fn league) #'> :key #'third)))


(defun do-percents (league)
  "Show percentage returns for each team in LEAGUE"
  (do-league-percents #'home-away-percentage-return league))

(defun do-loss-percents (league)
  "Show loss percentages for each team in LEAGUE"
  (do-league-percents #'home-away-loss-percentage-return league))

(defun do-home-percents (league)
  "Show home percentage returns for each team in LEAGUE"
  (do-league-percents #'home-percentage-return league))

(defun do-home-loss-percents (league)
  "Show home loss percentages for each team in LEAGUE"
  (do-league-percents #'home-loss-percentage-return league))

(defun do-away-percents (league)
  "Show away percentage returns for each team in LEAGUE"
  (do-league-percents #'away-percentage-return league))

(defun do-away-loss-percents (league)
  "Show away loss percentages for each team in LEAGUE"
  (do-league-percents #'away-loss-percentage-return league))

(defun do-draw-percents (league)
  "Show percentage returns for each team in LEAGUE"
  (do-league-percents #'draw-percentage-return league))

(defun do-home-draw-percents (league)
  "Show percentage returns for each team in LEAGUE"
  (do-league-percents #'home-draw-percentage-return league))

(defun do-away-draw-percents (league)
  "Show percentage returns for each team in LEAGUE"
  (do-league-percents #'away-draw-percentage-return league))

(defun do-last-six-wins-percents (league)
  (do-league-percents #'last-six-wins-percentage-return league))
(defun do-last-six-home-wins-percents (league)
  (do-league-percents #'last-six-home-wins-percentage-return league))
(defun do-last-six-away-wins-percents (league)
  (do-league-percents #'last-six-away-wins-percentage-return league))
(defun do-last-six-draw-percents (league)
  (do-league-percents #'last-six-draws-percentage-return league))

(defun do-last-six-wins-percents2 (league)
  (do-league-percents2 #'last-six-wins-percentage-return league))

(defun percents-all (fn)
  "Calculate percentage returns for all teams in all leagues"
  (let ((my-list nil))
    (dolist (league *leagues*)
      (dolist (team (league-percents fn (csv-filename league)))
        (push team my-list)))
    my-list))

(defun do-all-percents (fn)
  (percents-table
   (safe-sort (percents-all fn) #'< :key #'third)))

(defun do-percents-all ()
  "Show percentage returns for all teams"
  (do-all-percents #'home-away-percentage-return))

(defun do-loss-percents-all ()
  "Show loss percentages for all teams"
  (do-all-percents #'home-away-loss-percentage-return))

(defun do-home-percents-all ()
  "Show home percentage returns for all teams"
  (do-all-percents #'home-percentage-return))

(defun do-home-loss-percents-all ()
  "Show home loss percentages for all teams"
    (do-all-percents #'home-loss-percentage-return))

(defun do-away-percents-all ()
  "Show away percentage returns for all teams"
  (do-all-percents #'away-percentage-return))

(defun do-away-loss-percents-all ()
  "Show away loss percentages for all teams"
  (do-all-percents #'away-loss-percentage-return))

(defun do-draw-percents-all ()
  "Show draw percentage returns for all teams"
  (do-all-percents #'draw-percentage-return))

(defun do-home-draw-percents-all ()
  "Show home draw percentage returns for all teams"
  (do-all-percents #'home-draw-percentage-return))

(defun do-away-draw-percents-all ()
  "Show away draw percentage returns for all teams"
  (do-all-percents #'away-draw-percentage-return))

(defun do-over-percents-all ()
  "Show over 2.5 percentage returns for all teams"
  (do-all-percents #'over-percentage-return))

(defun do-home-over-percents-all ()
  "Show over 2.5 home percentage returns for all teams"
  (do-all-percents #'home-over-percentage-return))

(defun do-away-over-percents-all ()
  "Show over 2.5 away percentage returns for all teams"
  (do-all-percents #'away-over-percentage-return))

(defun do-under-percents-all ()
  "Show under 2.5 percentage returns for all teams"
  (do-all-percents #'under-percentage-return))

(defun do-home-under-percents-all ()
  "Show under 2.5 home percentage returns for all teams"
  (do-all-percents #'home-under-percentage-return))

(defun do-away-under-percents-all ()
  "Show under 2.5 away percentage returns for all teams"
  (do-all-percents #'away-under-percentage-return))

(defun do-last-six-wins-percents-all ()
  (do-all-percents #'last-six-wins-percentage-return))
(defun do-last-six-home-wins-percents-all ()
  (do-all-percents #'last-six-home-wins-percentage-return))
(defun do-last-six-away-wins-percents-all ()
  (do-all-percents #'last-six-away-wins-percentage-return))
(defun do-last-six-draw-percents-all ()
  (do-all-percents #'last-six-draws-percentage-return))

;; Show only top n teams in all leagues for each predicate

(defun do-top-percents (fn n)
  (percents-table
   (first-n n (safe-sort (percents-all fn)
				#'> :key #'third))))

(defun top-percents (&optional (n 10))
  "Show top win percentage returns for all teams"
  (do-top-percents #'home-away-percentage-return n))

(defun top-loss-percents (&optional (n 10))
  "Show top loss percentages for all teams"
  (do-top-percents #'home-away-loss-percentage-return n))

(defun top-home-percents (&optional (n 10))
  "Show top home percentage returns for all teams"
  (do-top-percents #'home-percentage-return n))

(defun top-home-loss-percents (&optional (n 10))
  "Show top home loss percentages for all teams"
  (do-top-percents #'home-loss-percentage-return n))

(defun top-away-percents (&optional (n 10))
  "Show top away percentage returns for all teams"
  (do-top-percents #'away-percentage-return n))

(defun top-away-loss-percents (&optional (n 10))
  "Show top away loss percentages for all teams"
  (do-top-percents #'away-loss-percentage-return n))

(defun top-draw-percents (&optional (n 10))
  "Show top draw percentage returns for all teams"
  (do-top-percents #'draw-percentage-return n))

(defun top-home-draw-percents (&optional (n 10))
  "Show top home draw percentage returns for all teams"
  (do-top-percents #'home-draw-percentage-return n))

(defun top-away-draw-percents (&optional (n 10))
  "Show top away draw percentage returns for all teams"
  (do-top-percents #'away-draw-percentage-return n))

(defun top-over-percents (&optional (n 10))
  "Show top over percentage returns for all teams"
  (do-top-percents #'over-percentage-return n))

(defun top-under-percents (&optional (n 10))
  "Show top under percentage returns for all teams"
  (do-top-percents #'under-percentage-return n))

(defun top-last-six-wins-percents (&optional (n 10))
  (do-top-percents #'last-six-wins-percentage-return n))
(defun top-last-six-home-win-percents (&optional (n 10))
  (do-top-percents #'last-six-home-wins-percentage-return n))
(defun top-last-six-away-wins-percents (&optional (n 10))
  (do-top-percents #'last-six-away-wins-percentage-return n))
(defun top-last-six-draw-percents (&optional (n 10))
  (do-top-percents #'last-six-draws-percentage-return n))

;; Returns stats

(defun print-header ()
  (format t "~7t|~12tWins~22t| ~26tLosses ~37t| ~41t Draws")
  (format t "~%---------------------------------------------------"))

(defun get-home-stats-detail (team)
  (format t "~% Homes | £~5,2f ~,2f% | £~5,2f ~,2f% | £~5,2f ~,2f%"
          (home-returns team) (home-percentage-return team)
		  (home-loss-returns team) (home-loss-percentage-return team)
		  (home-draw-returns team) (home-draw-percentage-return team)))
(defun get-away-stats-detail (team)
  (format t "~% Aways | £~5,2f ~,2f% | £~5,2f ~,2f% | £~5,2f ~,2f%"
          (away-returns team) (away-percentage-return team)
		  (away-loss-returns team) (away-loss-percentage-return team)
		  (away-draw-returns team) (away-draw-percentage-return team)))
(defun get-home-away-stats-detail (team)
  (format t "~% All   | £~5,2f ~,2f% | £~5,2f ~,2f% | £~5,2f ~,2f%"
          (home-away-returns team) (home-away-percentage-return team)
		  (home-away-loss-returns team) (home-away-loss-percentage-return team)
		  (draw-returns team) (draw-percentage-return team)))

(defun get-home-stats (team)
  (print-header)
  (get-home-stats-detail team))
(defun get-away-stats (team)
  (print-header)
  (get-away-stats-detail team))
(defun get-home-away-stats (team)
  (print-header)
  (get-home-away-stats-detail team))

(defun say-return-stats (team)
  "Show return stats for given TEAM"
  (print-header)
  (get-home-stats-detail team)
  (get-away-stats-detail team)
  (get-home-away-stats-detail team))

(defun percents-all2 (fn)
  (let ((my-list nil))
	(mapcar #'(lambda (league)
				(mapcar #'(lambda (team)
							(push team my-list))
						(league-percents fn (csv-filename league))))
			*leagues*)))
			
			
;;; Experimental (???) last-six routines 10/12/20

;; if going to change to recent (see below) start from here;
;;; last-six function -> recent-games
(defun get-last-six (fn team &optional (ngames 6))
  (let ((my-data (funcall fn team)))
	(last-n my-data ngames))) 

(defun last-six-homes (team &optional (ngames 6))
  (get-last-six #'homes team ngames))
(defun last-six-aways (team &optional (ngames 6))
  (get-last-six #'aways team ngames))
(defun last-six (team &optional (ngames 6))
  (get-last-six #'home-aways team ngames))

;; these work
;; first try
(defun say-last-six (team &key (odds nil) ((:games ngames) 6))
  (say (last-six team ngames) :odds odds))

;; second try
(defun do-say-last-six-games (team games-fn odds ngames)
  (say (funcall games-fn team ngames) :odds odds))
;; think above should be macro
(defmacro do-say-last-six-games-mac (team games-fn &key (odds nil) (ngames 6))
  `,(say (funcall `,games-fn `,team `,ngames) :odds `,odds))
;;(defun new-homes) ;; ? how to declare odds & ngames - not sure can :-(
;; unless by using params as per pcl ??

(defun say-last-six-homes2 (team &key (odds nil) ((:games ngames) 6))
  (do-say-last-six-games-mac team #'last-six-homes :odds odds :ngames ngames))
(defun say-last-six-homes (team &key (odds nil) ((:games ngames) 6))
  (do-say-last-six-games team #'last-six-homes odds ngames))
(defun say-last-six-aways (team &key (odds nil) ((:games ngames) 6))
  (do-say-last-six-games team #'last-six-aways odds ngames))
;; to here

;; maybe use recent-games/homes/aways to replace last-six/last-n ??
;; write macro to reduce above duplication ??

#|
(defmacro defsay (name team games-fn)
`(defun ,name (,team ,games-fn)
;;	 `,(say (funcall #',games-fn ,team))
`,(do-say-last-six-games team `,games-fn odds games)
))
(defun say-last-six-games2 (team games-fn odds ngames)
  (say (funcall games-fn team ngames) :odds odds))

;; = older

(defsay say-last-six-homes3 team last-six-homes)

(defun say-last-six-games2 (team games-fn odds ngames)
  (say (funcall games-fn team ngames) :odds odds))

(defmacro defsay (name team games-fn)
  `(defun ,name (,team ,games-fn &key (odds nil) (games 6))
	 `,(say (funcall ,games-fn ,team ,games) :odds ,odds)
;;	 `,(say-last-six-games2 team `,games-fn odds games)
	 ))

(defsay say-last-six-homes3 team #'last-six-homes)
|#

(defun do-say-last-six-games (team games-fn odds ngames)
  (say (funcall games-fn team ngames) :odds odds))

(defun say-last-six (team &key (odds nil) ((:games ngames) 6))
  (do-say-last-six-games team #'last-six odds ngames))
(defun say-last-six-homes (team &key (odds nil) ((:games ngames) 6))
  (do-say-last-six-games team #'last-six-homes odds ngames))
(defun say-last-six-aways (team &key (odds nil) ((:games ngames) 6))
  (do-say-last-six-games team #'last-six-aways odds ngames))

(defmacro do-say-last-six-games-mac (team games-fn &key (odds nil) (ngames 6))
  `(say (funcall ,games-fn ,team ,ngames) :odds ,odds))
(defun say-last-six2 (team &key (odds nil) (games 6))
  (do-say-last-six-games-mac team #'last-six :odds odds :ngames games))
(defun say-last-six-homes2 (team &key (odds nil) (games 6))
  (do-say-last-six-games-mac team #'last-six-homes :odds odds :ngames games))
(defun say-last-six-aways2 (team &key (odds nil) (games 6))
  (do-say-last-six-games-mac team #'last-six-aways :odds odds :ngames games))

(defmacro defsay (name (team games-fn))
  `(defun ,name (,team &key (odds nil) (games 6))
	(say (funcall ,games-fn ,team games) :odds odds)))

(defsay say-last-six3 (team #'last-six))
(defsay say-last-six-homes3 (team #'last-six-homes))
(defsay say-last-six-aways3 (team #'last-six-aways))

;; Returns per team
(defmacro defreturns (name odds-fn game-list)
  `(defun ,name (team)
	 (returns ,odds-fn ,game-list)))

(defreturns home-win-returns2
  #'home-odds (home-wins team))
(defreturns away-win-returns2
  #'away-odds (away-wins team))

(defmacro def%-return (name team games-fn returns-fn)
  `(defun ,name (,team)
	 (percentage-return ,games-fn ,returns-fn ,team)))

(def%-return home-win-percentage-return2
  team #'homes #'home-win-returns)
(def%-return away-win-percentage-return2
  team #'aways #'away-win-returns)

;; trying to get rid of get-all-btts but doesnt work
(defun sort-btts2 (leagues &optional (n 10))
  (let ((my-list nil))
	(first-n n (sort
				(mapcar #'(lambda (league)
							(setf my-list (append my-list (get-btts (csv-filename league)))))
						leagues)
				#'> :key #'second))))

;; **************************************************************
;;
;;  DSL Returns
;;

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

(defun percentage-return (games-fn returns-fn team)
  (let ((ngames (length (funcall games-fn team))))
	(cond ((zerop ngames) (values 0 0 0))
		  (t (let ((team-return (funcall returns-fn team)))
			   (values (/ team-return ngames)
					   team-return
					   ngames))))))

;; Returns per team

(defun home-win-returns (team)
  (returns #'home-odds (home-wins team)))
(defun away-win-returns (team)
  (returns #'away-odds (away-wins team)))

(defun home-win-percentage-return (team)
  (percentage-return #'homes #'home-win-returns team))
(defun away-win-percentage-return (team)
  (percentage-return #'aways #'away-win-returns team))

(defun home-loss-returns (team)
  (returns #'away-odds (home-defeats team)))
(defun away-loss-returns (team)
  (returns #'home-odds (away-defeats team)))

(defun home-loss-percentage-return (team)
  (percentage-return #'homes #'home-loss-returns team))
(defun away-loss-percentage-return (team)
  (percentage-return #'aways #'away-loss-returns team))

(defun home-away-win-returns (team)
  (+ (home-win-returns team)
     (away-win-returns team)))
(defun home-away-loss-returns (team)
  (+ (home-loss-returns team)
     (away-loss-returns team)))

(defun home-away-win-percentage-return (team)
  (percentage-return #'home-aways #'home-away-win-returns team))
(defun home-away-loss-percentage-return (team)
  (percentage-return #'home-aways #'home-away-loss-returns team))

(defun draw-returns (team)
  (returns #'draw-odds (draws team)))
(defun home-draw-returns (team)
  (returns #'draw-odds (home-draws team)))
(defun away-draw-returns (team)
  (returns #'draw-odds (away-draws team)))

(defun draw-percentage-return (team)
  (percentage-return #'home-aways #'draw-returns team))
(defun home-draw-percentage-return (team)
  (percentage-return #'homes #'home-draw-returns team))
(defun away-draw-percentage-return (team)
  (percentage-return #'aways #'away-draw-returns team))

(defun home-away-over-returns (team)
  (returns #'over-odds (home-away-overs team)))
(defun home-over-returns (team)
  (returns #'over-odds (home-overs team)))
(defun away-over-returns (team)
  (returns #'over-odds (away-overs team)))

(defun over-percentage-return (team)
  (percentage-return #'home-aways #'home-away-over-returns team))
(defun home-over-percentage-return (team)
  (percentage-return #'homes #'home-over-returns team))
(defun away-over-percentage-return (team)
  (percentage-return #'aways #'away-over-returns team))

(defun home-away-under-returns (team)
  (returns #'under-odds (home-away-unders team)))
(defun home-under-returns (team)
  (returns #'under-odds (home-unders team)))
(defun away-under-returns (team)
  (returns #'under-odds (away-unders team)))

(defun under-percentage-return (team)
  (percentage-return #'home-aways #'home-away-under-returns team))
(defun home-under-percentage-return (team)
  (percentage-return #'homes #'home-under-returns team))
(defun away-under-percentage-return (team)
  (percentage-return #'aways #'away-under-returns team))

(defun last-six-win-returns (team)
  (ha-returns team #'home-away-odds (wins-in-last-six team)))
(defun last-six-homes-return (team)
  (returns #'home-odds (wins-in-last-six-homes team)))
(defun last-six-aways-return (team)
  (returns #'away-odds (wins-in-last-six-aways team)))

(defun last-six-win-percentage-return (team)
  (percentage-return #'last-six #'last-six-win-returns team))
(defun last-six-home-win-percentage-return (team)
  (percentage-return #'last-six-homes #'last-six-homes-return team))
(defun last-six-away-win-percentage-return (team)
  (percentage-return #'last-six-aways #'last-six-aways-return team))

(defun last-six-loss-returns (team)
  (ha-returns team #'home-away-lost-odds (defeats-in-last-six team)))
(defun last-six-draw-return (team)
  (returns #'draw-odds (draws-in-last-six team)))

(defun last-six-loss-percentage-return (team)
  (percentage-return #'last-six #'last-six-loss-returns team))
(defun last-six-draw-percentage-return (team)
  (percentage-return #'last-six #'last-six-draw-return team))

(defun last-six-over-returns (team)
  (returns #'over-odds (last-six-overs team)))
(defun last-six-over-percentage-return (team)
  (percentage-return #'last-six #'last-six-over-returns team))
(defun last-six-under-returns (team)
  (returns #'under-odds (last-six-unders team)))
(defun last-six-under-percentage-return (team)
  (percentage-return #'last-six #'last-six-under-returns team))

(defun last-six-home-over-returns (team)
  (returns #'over-odds (last-six-home-overs team)))
(defun last-six-home-over-percentage-return (team)
  (percentage-return #'last-six-homes #'last-six-home-over-returns team))
(defun last-six-home-under-returns (team)
  (returns #'under-odds (last-six-home-unders team)))
(defun last-six-home-under-percentage-return (team)
  (percentage-return #'last-six-homes #'last-six-home-under-returns team))

(defun last-six-away-over-returns (team)
  (returns #'over-odds (last-six-away-overs team)))
(defun last-six-away-over-percentage-return (team)
  (percentage-return #'last-six-aways #'last-six-away-over-returns team))
(defun last-six-away-under-returns (team)
  (returns #'under-odds (last-six-away-unders team)))
(defun last-six-away-under-percentage-return (team)
  (percentage-return #'last-six-aways #'last-six-away-under-returns team))

;; Returns per league

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

(defun do-league-percents (fn league)
  (percents-table
   (safe-sort (league-percents fn league) #'> :key #'fifth)))

(defun do-win-percents (league)
  "Show percentage returns for each team in LEAGUE"
  (do-league-percents #'home-away-win-percentage-return league))

(defun do-loss-percents (league)
  "Show loss percentages for each team in LEAGUE"
  (do-league-percents #'home-away-loss-percentage-return league))

(defun do-home-win-percents (league)
  "Show home percentage returns for each team in LEAGUE"
  (do-league-percents #'home-win-percentage-return league))

(defun do-home-loss-percents (league)
  "Show home loss percentages for each team in LEAGUE"
  (do-league-percents #'home-loss-percentage-return league))

(defun do-away-win-percents (league)
  "Show away percentage returns for each team in LEAGUE"
  (do-league-percents #'away-win-percentage-return league))

(defun do-away-loss-percents (league)
  "Show away loss percentages for each team in LEAGUE"
  (do-league-percents #'away-loss-percentage-return league))

(defun do-draw-percents (league)
  "Show percentage returns for each team in LEAGUE"
  (do-league-percents #'draw-percentage-return league))

(defun do-home-draw-percents (league)
  "Show percentage returns for each team in LEAGUE"
  (do-league-percents #'home-draw-percentage-return league))

(defun do-away-draw-percents (league)
  "Show percentage returns for each team in LEAGUE"
  (do-league-percents #'away-draw-percentage-return league))

(defun do-over-percents (league)
  (do-league-percents #'over-percentage-return league))
(defun do-under-percents (league)
  (do-league-percents #'under-percentage-return league))

(defun do-last-six-win-percents (league)
  (do-league-percents #'last-six-win-percentage-return league))
(defun do-last-six-home-win-percents (league)
  (do-league-percents #'last-six-home-win-percentage-return league))
(defun do-last-six-away-wins-percents (league)
  (do-league-percents #'last-six-away-win-percentage-return league))
(defun do-last-six-draw-percents (league)
  (do-league-percents #'last-six-draw-percentage-return league))

(defun last-six-over-percents (league)
  (do-league-percents #'last-six-over-percentage-return league))
(defun last-six-home-over-percents (league)
  (do-league-percents #'last-six-home-over-percentage-return league))
(defun last-six-away-over-percents (league)
  (do-league-percents #'last-six-away-over-percentage-return league))

(defun last-six-under-percents (league)
  (do-league-percents #'last-six-under-percentage-return league))
(defun last-six-home-under-percents (league)
  (do-league-percents #'last-six-home-under-percentage-return league))
(defun last-six-away-under-percents (league)
  (do-league-percents #'last-six-away-under-percentage-return league))

(defun percents-all (fn)
  "Calculate percentage returns for all teams in all leagues"
  (let ((my-list nil))
    (dolist (league *leagues*)
      (dolist (team (league-percents fn (csv-filename league)))
        (push team my-list)))
    my-list))

(defun do-all-percents (fn)
  (percents-table
   (safe-sort (percents-all fn) #'< :key #'fifth)))

(defun do-win-percents-all ()
  "Show percentage returns for all teams"
  (do-all-percents #'home-away-win-percentage-return))

(defun do-loss-percents-all ()
  "Show loss percentages for all teams"
  (do-all-percents #'home-away-loss-percentage-return))

(defun do-home-win-percents-all ()
  "Show home percentage returns for all teams"
  (do-all-percents #'home-win-percentage-return))

(defun do-home-loss-percents-all ()
  "Show home loss percentages for all teams"
  (do-all-percents #'home-loss-percentage-return))

(defun do-away-win-percents-all ()
  "Show away percentage returns for all teams"
  (do-all-percents #'away-win-percentage-return))

(defun do-away-loss-percents-all ()
  "Show away loss percentages for all teams"
  (do-all-percents #'away-loss-percentage-return))

(defun do-draw-percents-all ()
  "Show draw percentage returns for all teams"
  (do-all-percents #'draw-percentage-return))

(defun do-home-draw-percents-all ()
  "Show home draw percentage returns for all teams"
  (do-all-percents #'home-draw-percentage-return))

(defun do-away-draw-percents-all ()
  "Show away draw percentage returns for all teams"
  (do-all-percents #'away-draw-percentage-return))

(defun do-over-percents-all ()
  "Show over 2.5 percentage returns for all teams"
  (do-all-percents #'over-percentage-return))

(defun do-home-over-percents-all ()
  "Show over 2.5 home percentage returns for all teams"
  (do-all-percents #'home-over-percentage-return))

(defun do-away-over-percents-all ()
  "Show over 2.5 away percentage returns for all teams"
  (do-all-percents #'away-over-percentage-return))

(defun do-under-percents-all ()
  "Show under 2.5 percentage returns for all teams"
  (do-all-percents #'under-percentage-return))

(defun do-home-under-percents-all ()
  "Show under 2.5 home percentage returns for all teams"
  (do-all-percents #'home-under-percentage-return))

(defun do-away-under-percents-all ()
  "Show under 2.5 away percentage returns for all teams"
  (do-all-percents #'away-under-percentage-return))

(defun do-last-six-win-percents-all ()
  (do-all-percents #'last-six-win-percentage-return))
(defun do-last-six-home-win-percents-all ()
  (do-all-percents #'last-six-home-win-percentage-return))
(defun do-last-six-away-win-percents-all ()
  (do-all-percents #'last-six-away-win-percentage-return))
(defun do-last-six-draw-percents-all ()
  (do-all-percents #'last-six-draw-percentage-return))
(defun do-last-six-loss-percents-all ()
  (do-all-percents #'last-six-loss-percentage-return))

(defun do-last-six-over-percents-all ()
  (do-all-percents #'last-six-over-percentage-return))
(defun do-last-six-home-over-percents-all ()
  (do-all-percents #'last-six-home-over-percentage-return))
(defun do-last-six-away-over-percents-all ()
  (do-all-percents #'last-six-away-over-percentage-return))

(defun do-last-six-under-percents-all ()
  (do-all-percents #'last-six-under-percentage-return))
(defun do-last-six-home-under-percents-all ()
  (do-all-percents #'last-six-home-under-percentage-return))
(defun do-last-six-away-under-percents-all ()
  (do-all-percents #'last-six-away-under-percentage-return))

;; Show only top n teams in all leagues for each predicate

(defun do-top-percents (fn n)
  (percents-table
   (first-n n (safe-sort (percents-all fn)
				#'> :key #'fifth))))

(defun top-win-percents (&optional (n 10))
  "Show top win percentage returns for all teams"
  (do-top-percents #'home-away-win-percentage-return n))

(defun top-loss-percents (&optional (n 10))
  "Show top loss percentages for all teams"
  (do-top-percents #'home-away-loss-percentage-return n))

(defun top-home-win-percents (&optional (n 10))
  "Show top home percentage returns for all teams"
  (do-top-percents #'home-win-percentage-return n))

(defun top-home-loss-percents (&optional (n 10))
  "Show top home loss percentages for all teams"
  (do-top-percents #'home-loss-percentage-return n))

(defun top-away-win-percents (&optional (n 10))
  "Show top away percentage returns for all teams"
  (do-top-percents #'away-win-percentage-return n))

(defun top-away-loss-percents (&optional (n 10))
  "Show top away loss percentages for all teams"
  (do-top-percents #'away-loss-percentage-return n))

(defun top-draw-percents (&optional (n 10))
  "Show top draw percentage returns for all teams"
  (do-top-percents #'draw-percentage-return n))

(defun top-home-draw-percents (&optional (n 10))
  "Show top home draw percentage returns for all teams"
  (do-top-percents #'home-draw-percentage-return n))

(defun top-away-draw-percents (&optional (n 10))
  "Show top away draw percentage returns for all teams"
  (do-top-percents #'away-draw-percentage-return n))

(defun top-over-percents (&optional (n 10))
  "Show top over percentage returns for all teams"
  (do-top-percents #'over-percentage-return n))

(defun top-under-percents (&optional (n 10))
  "Show top under percentage returns for all teams"
  (do-top-percents #'under-percentage-return n))

(defun top-last-six-win-percents (&optional (n 10))
  (do-top-percents #'last-six-win-percentage-return n))
(defun top-last-six-home-win-percents (&optional (n 10))
  (do-top-percents #'last-six-home-win-percentage-return n))
(defun top-last-six-away-win-percents (&optional (n 10))
  (do-top-percents #'last-six-away-win-percentage-return n))
(defun top-last-six-draw-percents (&optional (n 10))
  (do-top-percents #'last-six-draw-percentage-return n))
(defun top-last-six-loss-percents (&optional (n 10))
  (do-top-percents #'last-six-loss-percentage-return n))

(defun top-last-six-over-percents (&optional (n 10))
  (do-top-percents #'last-six-over-percentage-return n))
(defun top-last-six-home-over-percents (&optional (n 10))
  (do-top-percents #'last-six-home-over-percentage-return n))
(defun top-last-six-away-over-percents (&optional (n 10))
  (do-top-percents #'last-six-away-over-percentage-return n))

(defun top-last-six-under-percents (&optional (n 10))
  (do-top-percents #'last-six-under-percentage-return n))
(defun top-last-six-home-under-percents (&optional (n 10))
  (do-top-percents #'last-six-home-under-percentage-return n))
(defun top-last-six-away-under-percents (&optional (n 10))
  (do-top-percents #'last-six-away-under-percentage-return n))

(defun say (games &key (odds nil))
  (mapcar #'(lambda (game)
			  (if odds 
				  (say-game-with-odds game)
				  (say-game game)))
		  games)
  t)
  
 (defmacro do-say (fn-name games-fn)
  `(defun ,fn-name (team &key (odds nil))
	 (say (funcall ,games-fn team) :odds odds)))

(defun result-list1 (team game)
"Amend list to change result from [H A D] to [W L D] for the given TEAM"
  (append (subseq game 0 5)
		  (cons (get-result team game)
				(subseq game 6))))
		
(defun result-list (team game)
  "Amend list to transform result column from [H A D] to [W L D] for the given TEAM"
  (cond ((is-draw game) game)
		(t (append (subseq game 0 5)
				   (cons (win-lose-result team game)
						 (subseq game 6))))))

(defun say (team games &key (odds nil))
  (mapcar #'(lambda (game)
			  (if odds 
				  (say-game-with-odds (result-list team game))
				  (say-game (result-list team game))))
		  games)
  t)

(do-say say-home-overs #'home-overs)
(do-say say-away-overs #'away-overs)
(do-say say-home-away-overs #'home-away-overs)

(do-say say-home-unders #'home-unders)
(do-say say-away-unders #'away-unders)
(do-say say-home-away-unders #'home-away-unders)

(do-say say-last-six-overs #'last-six-overs)
(do-say say-last-six-home-overs #'last-six-home-overs)
(do-say say-last-six-away-overs #'last-six-away-overs)

(do-say say-last-six-unders #'last-six-unders)
(do-say say-last-six-home-unders #'last-six-home-unders)
(do-say say-last-six-away-unders #'last-six-away-unders)

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

;;==================================================================================
13/11/21

;; ***************************************************************

;; DSL
;; Accessor macros and functions for each game in *db*
;;

(proclaim '(inline date home-team away-team home-score away-score result
			home-odds away-odds draw-odds over-odds under-odds))

(defun date (game) (first game))
(defun home-team (game) (second game))
(defun away-team (game) (third game))
(defun home-score (game) (parse-integer (fourth game)))
(defun away-score (game) (parse-integer (fifth game)))
(defun result (game) (sixth game))

(defun home-odds (game) (parse-float (seventh game)))
(defun away-odds (game) (parse-float (ninth game)))
(defun draw-odds (game) (parse-float (eighth game)))

(defun over-odds (game)
  (if (> (length game) 9)
	  (parse-float (tenth game))
	  *summer-over-under-odds*)) ;; work-around for summer files

(defun under-odds (game)
  (if (> (length game) 9)
	  (parse-float (nth 10 game))
	  *summer-over-under-odds*))

(defun home (team game) (equal team (home-team game)))
(defun away (team game) (equal team (away-team game)))
(defun home-away (team game)
  (or (equal team (home-team game))
      (equal team (away-team game))))

;; Accessor functions for *fixtures*
(defun fleague (game) (string-downcase (second game)))
(defun fhome (game) (third game))
(defun faway (game) (fourth game))

(defun find-league (team)
  "Find csv-league for TEAM"
  (dolist (league *teams*)
    (when (member team (second league) :test #'string-equal)
	  (return-from find-league (string-downcase (first league))))))

(proclaim '(inline csv-data-league-name csv-data-results))
(defun csv-data-league-name (data) (first (first data)))
(defun csv-data-results (data) (rest (first data)))

(defun get-league (csv-league)
  (labels ((get-league-data (csv-league db)
			 (cond ((null db) nil)
				   ((string-equal csv-league (csv-data-league-name db))
					(csv-data-results db))
				   (t (get-league-data csv-league (rest db))))))
	(get-league-data csv-league *db*)))

(defun get-games (fn team)
  (remove-if-not
    #'(lambda (game)
        (funcall fn team game))
    (get-league (find-league team))))

(defun homes (team)
  (get-games #'home team))
(defun aways (team)
  (get-games #'away team))
(defun home-aways (team)
  (get-games #'home-away team))

(defun get-results (result list)
  (remove-if-not
   #'(lambda (game)
	   (equal (result game) result))
   list))

(defun home-wins (team)
  (get-results "H" (homes team)))
(defun home-draws (team)
  (get-results "D" (homes team)))
(defun home-defeats (team)
  (get-results "A" (homes team)))

(defun away-wins (team)
  (get-results "A" (aways team)))
(defun away-draws (team)
  (get-results "D" (aways team)))
(defun away-defeats (team)
  (get-results "H" (aways team)))

(defun first-n (n my-list)
  (let* ((len (length my-list))
         (start-elem (if (> len n)
                         (- len n) 0)))
    (butlast my-list start-elem)))

(defun last-n (data n)
  (let* ((len (length data))
		 (start-elem (if (>= n len)
						 0 (- len n))))
	(nthcdr start-elem data)))

(defun get-last-six (fn team &optional (ngames 6))
  (let ((my-data (funcall fn team)))
	(last-n my-data ngames))) 

(defun last-six-homes (team &optional (ngames 6))
  (get-last-six #'homes team ngames))
(defun last-six-aways (team &optional (ngames 6))
  (get-last-six #'aways team ngames))
(defun last-six (team &optional (ngames 6))
  (get-last-six #'home-aways team ngames))

(defun last-n-homes (team n)
  (last-six-homes team n))
(defun last-n-aways (team n)
  (last-six-aways team n))
(defun last-n-games (team n)
  (last-six team n))

(defun last-six-home-wins (team &optional (ngames 6))
  (get-last-six #'home-wins team ngames))
(defun last-six-home-draws (team &optional (ngames 6))
  (get-last-six #'home-draws team ngames))
(defun last-six-home-defeats (team &optional (ngames 6))
  (get-last-six #'home-defeats team ngames))

(defun last-six-away-wins (team &optional (ngames 6))
  (get-last-six #'away-wins team ngames))
(defun last-six-away-draws (team &optional (ngames 6))
  (get-last-six #'away-draws team ngames))
(defun last-six-away-defeats (team &optional (ngames 6))
  (get-last-six #'away-defeats team ngames))

(defun get-ha-result (fn team)
  (remove-if-not
   #'(lambda (game)
       (funcall fn team game))
   (home-aways team)))

(defun home-away-win-result (team game)
  (or (and (home team game)
           (equal (result game) "H"))
      (and (away team game)
           (equal (result game) "A"))))

(defun home-away-lost-result (team game)
  (or (and (home team game)
           (equal (result game) "A"))
      (and (away team game)
           (equal (result game) "H"))))

(defun not-home-away-win-result (team game)
  (not (home-away-win-result team game)))

(defun home-away-odds (team game)
  (cond ((equal team (home-team game))
		 (home-odds game))
		((equal team (away-team game))
		 (away-odds game))
		(t 0)))

(defun home-away-lost-odds (team game)
  (cond ((equal team (home-team game))
		 (away-odds game))
		((equal team (away-team game))
		 (home-odds game))
		(t 0)))

(defun home-away-draw-result (team game)
  (and (home-away team game)
       (equal (result game) "D")))

(defun wins (team)
  (get-ha-result #'home-away-win-result team))
(defun defeats (team)
  (get-ha-result #'home-away-lost-result team))
(defun draws (team)
  (get-ha-result #'home-away-draw-result team))

(defun last-six-wins (team &optional (ngames 6))
  (get-last-six #'wins team ngames))
(defun last-six-draws (team &optional (ngames 6))
  (get-last-six #'draws team ngames))
(defun last-six-defeats (team &optional (ngames 6))
  (get-last-six #'defeats team ngames))

(defun total-wins (team)
  (length (wins team)))
(defun total-defeats (team)
  (length (defeats team)))
(defun total-draws (team)
  (length (draws team)))
(defun total-home-games (team)
  (length (homes team)))
(defun total-away-games (team)
  (length (aways team)))
(defun total-games (team)
  (length (home-aways team)))

(defun home-away-unwins (team game)
  (or (home-away-lost-result team game)
	  (home-away-draw-result team game)))
(defun home-away-undefeats (team game)
  (or (home-away-win-result team game)
	  (home-away-draw-result team game)))
(defun home-away-undraws (team game)
  (not (home-away-draw-result team game)))

(defun unwins (team)
  (get-ha-result #'home-away-unwins team))
(defun undefeats (team)
  (get-ha-result #'home-away-undefeats team))
(defun undraws (team)
  (get-ha-result #'home-away-undraws team))

(defun is-win (team game)
  (home-away-win-result team game))
(defun is-defeat (team game)
  (home-away-lost-result team game))
(defun is-draw (game)
  (equal (result game) "D"))

(defun get-result (team game)
  (cond ((is-draw game) "D")
		((is-win team game) "W")
		(t "L")))

(defun match-goals (game)
  (+ (home-score game)
	 (away-score game)))

(defun is-over (game &optional (n 2.5))
  (> (match-goals game) n))
(defun is-under (game &optional (n 2.5))
  (< (match-goals game) n))

(defun get-over-unders (gt-lt-fn fn team n)
  "Returns a list of games either over or under (specified by GT-LT-FN)
   N goals for TEAM from a list of home/away/both games specified by FN"
  (remove-if-not #'(lambda (game)
					 (funcall gt-lt-fn
							  (match-goals game) n))
				 (funcall fn team)))

(defun home-away-overs (team &optional (n 2.5))
  (get-over-unders #'> #'home-aways team n))
(defun home-overs (team &optional (n 2.5))
  (get-over-unders #'> #'homes team n))
(defun away-overs (team &optional (n 2.5))
  (get-over-unders #'> #'aways team n))

(defun home-away-unders (team &optional (n 2.5))
  (get-over-unders #'< #'home-aways team n))
(defun home-unders (team &optional (n 2.5))
  (get-over-unders #'< #'homes team n))
(defun away-unders (team &optional (n 2.5))
  (get-over-unders #'< #'aways team n))

(defun last-six-overs (team &optional (n 2.5))
  (get-over-unders #'> #'last-six team n))
(defun last-six-home-overs (team &optional (n 2.5))
  (get-over-unders #'> #'last-six-homes team n))
(defun last-six-away-overs (team &optional (n 2.5))
  (get-over-unders #'> #'last-six-aways team n))

(defun last-six-unders (team &optional (n 2.5))
  (get-over-unders #'< #'last-six team n))
(defun last-six-home-unders (team &optional (n 2.5))
  (get-over-unders #'< #'last-six-homes team n))
(defun last-six-away-unders (team &optional (n 2.5))
  (get-over-unders #'< #'last-six-aways team n))

(defun get-home-away-result (result-fn games-fn team)
  (remove-if-not
   #'(lambda (game)
	   (funcall result-fn team game))
   (funcall games-fn team)))

(defun wins-in-last-six (team)
  (get-home-away-result #'home-away-win-result #'last-six team))
(defun defeats-in-last-six (team)
  (get-home-away-result #'home-away-lost-result #'last-six team))
(defun draws-in-last-six (team)
  (get-home-away-result #'home-away-draw-result #'last-six team))

(defun wins-in-last-six-homes (team)
  (get-home-away-result #'home-away-win-result #'last-six-homes team))
(defun defeats-in-last-six-homes (team)
  (get-home-away-result #'home-away-lost-result #'last-six-homes team))
(defun draws-in-last-six-homes (team)
  (get-home-away-result #'home-away-draw-result #'last-six-homes team))

(defun wins-in-last-six-aways (team)
  (get-home-away-result #'home-away-win-result #'last-six-aways team))
(defun defeats-in-last-six-aways (team)
  (get-home-away-result #'home-away-lost-result #'last-six-aways team))
(defun draws-in-last-six-aways (team)
  (get-home-away-result #'home-away-draw-result #'last-six-aways team))

(defun get-league-home-wins (games-list)
  (get-results "H" games-list))
(defun get-league-away-wins (games-list)
  (get-results "A" games-list))
(defun get-league-draws (games-list)
  (get-results "D" games-list))

(defun get-last-n-over-unders (gt-lt-fn fn team games goals)
  "Returns a list of games either over or under (specified by GT-LT-FN)
   GOALS goals for TEAM from a list of last GAMES played by home/away/both games specified by FN"
  (remove-if-not #'(lambda (game)
					 (funcall gt-lt-fn
							  (match-goals game) goals))
				 (funcall fn team games)))

(defun last-n-overs (team &key (games 6) (goals 2.5))
  (get-last-n-over-unders #'> #'last-n-games team games goals))
(defun last-n-home-overs (team &key (games 6) (goals 2.5))
  (get-last-n-over-unders #'> #'last-n-homes team games goals))
(defun last-n-away-overs (team &key (games 6) (goals 2.5))
  (get-last-n-over-unders #'> #'last-n-aways team games goals))

(defun last-n-unders (team &key (games 6) (goals 2.5))
  (get-last-n-over-unders #'< #'last-n-games team games goals))
(defun last-n-home-unders (team &key (games 6) (goals 2.5))
  (get-last-n-over-unders #'< #'last-n-homes team games goals))
(defun last-n-away-unders (team &key (games 6) (goals 2.5))
  (get-last-n-over-unders #'< #'last-n-aways team games goals))

(defun get-teams (csv-league &optional (teams *teams*))
  "Returns a list of teams in the given CSV-LEAGUE"
  (cond ((null teams) nil)
        ((string-equal csv-league (csv-filename (first teams))) ; league id
		 (second (first teams))) ;; list of league teams
        (t (get-teams csv-league (rest teams)))))

(defun count-all-league-results ()
  (mapcar #'(lambda (league)
			  (let* ((league-name (csv-league-name league))
					 (games (get-league (csv-filename league)))
					 (num-games (length games))
					 (home-wins (length (get-league-home-wins games)))
					 (away-wins (length (get-league-away-wins games)))
					 (draws (length (get-league-draws games))))
				(format t "~%~%~a Home Wins : ~3d ~$%" league-name home-wins (calc-percent num-games home-wins))
				(format t "~%~a Away Wins : ~3d ~$%" league-name away-wins (calc-percent num-games away-wins))
				(format t "~%~a Draws     : ~3d ~$%" league-name draws (calc-percent num-games draws))))
		  *leagues*))

(defun lengths (args)
  "Return the a list of lengths of each list in a given list of lists
   eg (lengths '((1 2 3) (9 8 7 6) (1 2 3 4 5 6 7 8 9))) returns (3 4 9)"
  (mapcar #'(lambda (arg)
			  (length arg))
		  args))

(defun home-away-record (team)
  (destructuring-bind (home-wins away-wins home-draws away-draws home-defeats away-defeats)
	  (lengths (list (home-wins team)
					 (away-wins team)
					 (home-draws team)
					 (away-draws team)
					 (home-defeats team)
					 (away-defeats team)))
	(format t "~%Home Wins    : ~a~40tAway Wins    : ~a~%Home Draws   : ~a~40tAway Draws   : ~a~%Home Defeats : ~a~40tAway Defeats : ~a"
			home-wins away-wins home-draws away-draws home-defeats away-defeats)))


****************8
18/01/22

(defmacro do-get-results (fn-name games-fn result)
  `(defun ,fn-name (team)
	 (remove-if-not #'(lambda (game)
						(equal (result game) ,result))
					(funcall ,games-fn team))))

(do-get-results home-wins #'homes "H")
(do-get-results home-draws #'homes "D")
(do-get-results home-defeats #'homes "A")

(do-get-results away-wins #'aways "A")
(do-get-results away-draws #'aways "D")
(do-get-results away-defeats #'aways "H")

(defmacro do-get-ha-result (fn-name result-fn)
  `(defun ,fn-name (team &optional (games-fn #'home-aways))
	 (remove-if-not #'(lambda (game)
						(funcall ,result-fn team game))
					(funcall games-fn team))))

(do-get-ha-result wins #'home-away-win-result)
(do-get-ha-result defeats #'home-away-defeat-result)
(do-get-ha-result draws #'home-away-draw-result)

(do-get-last-six last-six-wins #'wins)
(do-get-last-six last-six-draws #'draws)
(do-get-last-six last-six-defeats #'defeats)

;;Need to write is-home-win (equal (result game "H")). is-away-win, is-draw,
;;then pass these to this macro, for above funcs (wins etc) pass home-away-win-result
;;start again in in-progress

;; good idea but won;t work- diff signatures - prob better using get-result above,
;; which ends up calling hawin-result so see beloe,,
;;is it better to always use hawinresult AS result-fn, just pass in gamesfn ??fuk knows :-)

(defmacro do-get-ha-result2 (fn-name result-fn games-fn)
  `(defun ,fn-name (team)
	 (remove-if-not #'(lambda (game)
						(funcall ,result-fn team game))
					(funcall ,games-fn team))))

(do-get-ha-result2 wins-in-last-six #'home-away-win-result #'last-six)
(do-get-ha-result2 defeats-in-last-six #'home-away-defeat-result #'last-six)
(do-get-ha-result2 draws-in-last-six #'home-away-draw-result #'last-six)

(do-get-ha-result2 wins-in-last-six-homes #'home-away-win-result #'last-six-homes)
(do-get-ha-result2 defeats-in-last-six-homes #'home-away-defeat-result #'last-six-homes)
(do-get-ha-result2 draws-in-last-six-homes #'home-away-draw-result #'last-six-homes)

(do-get-ha-result2 wins-in-last-six-aways #'home-away-win-result #'last-six-aways)
(do-get-ha-result2 defeats-in-last-six-aways #'home-away-defeat-result #'last-six-aways)
(do-get-ha-result2 draws-in-last-six-aways #'home-away-draw-result #'last-six-aways)

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
					  (calc-percent stake returns) ;; do this for all other (calc-percent stake returns)
;					  (my-round (* 100 (/ returns stake)) 0.01)
)
				my-list))))

	(sort my-list #'> :key #'fifth)))

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
					(over-odds game) (under-odds game)
 					stake returns)
			  my-list)))

	(values (reverse my-list)
			stake
			returns)))
			
(defun do-series2 (s games-fn result-fn odds-fn)
  "Returns a list of all teams sorted by their returns using series S (using calc-stake)
   given the games returned by GAMES-FN which match results from RESULT-FN at odds ODDS-FN"
  
  (let ((my-list nil))
	(with-all-teams (team *leagues*)
	  (let ((stake 0)
			(returns 0)
			(result ""))
		(series-reset s)
		(dolist (game (funcall games-fn team))
		  (let* ((odds (funcall odds-fn team game))
				 (current (if (>= odds 2)
							  (series-current s)
							  (calc-new-stake (series-current s) odds))))
			(+= stake current)
			(cond ((funcall result-fn team game)
				   (+= returns (* current odds))
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

(defun do-team-series-calc2 (team s games-fn result-fn odds-fn)
  "Returns details of a TEAM with series S, using games returned from GAMES-FN
   which match RESULT-FN at odds ODDS-FN"
  
  (let ((my-list nil)
		(stake 0)
		(returns 0)
		(result ""))

	(series-reset s)
	(dolist (game (funcall games-fn team))
	  (let* ((odds (funcall odds-fn team game))
			 (current (if (>= odds 2)
						  (series-current s)
						  (calc-new-stake (series-current s) odds))))
		(+= stake current)
		(cond ((funcall result-fn team game)
			   (+= returns (* current (funcall odds-fn team game)))
			   (setf result "W"))
			  (t (setf result "L")))
		(series-update s result)
		(push (list (date game) (home-team game) (away-team game)
					current result
					(home-odds game) (draw-odds game) (away-odds game)
					(over-odds game) (under-odds game)
 					stake returns)
			  my-list)))

	(values (reverse my-list)
			stake
			returns)))

(defmacro do-get-results (fn-name games-fn)
  `(defun ,fn-name (team result-fn)
	 (remove-if-not #'(lambda (game)
						(funcall result-fn team game))
					(funcall ,games-fn team))))

(defmacro do-get-n-results (fn-name games-fn)
  `(defun ,fn-name (team result-fn &optional (ngames 6))
	 (remove-if-not #'(lambda (game)
						(funcall result-fn team game))
					(funcall ,games-fn team ngames))))

(do-get-n-results with-last-n #'last-n-games)
(do-get-n-results with-last-n-homes #'last-n-homes)
(do-get-n-results with-last-n-aways #'last-n-aways)
			

	
(defun do-odds (&optional (expects *expects*))
  "Calculates odds for each game from goal expectancy values"
  (dolist (game expects)
    (let ((ds (make-instance 'my-odds :size 10)))
      (calc-game ds (game-home-goals game) (game-away-goals game))
	  (setf (values (game-home-odds game)
					(game-draw-odds game)
					(game-away-odds game)
					(game-over-odds game)
					(game-under-odds game))
			(vget-odds ds))))
  
  (format t "~%")
  (print-game-odds (safe-sort expects
                     #'< :key #'game-under-odds)))	
	
(defun do-odds (&key (games *expects*) (cmp-fn #'game-under-odds))
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
  
  (format t "~%")
  (print-game-odds (safe-sort games
                     #'< :key cmp-fn)))	
					 
(defun under-expects (&optional (goals 1))
  (let ((my-list nil))
	(mapcar #'(lambda (game)
				(when (and (< (game-home-goals game) goals)
						   (< (game-away-goals game) goals))
				  (push game my-list)))
			*expects*)

	(mapcar #'(lambda (game)
				(format t "~% ~3a : ~a v ~a ~37tHome : ~,2f Away : ~,2f - ~,2f"
						(string-upcase (game-league game))
						(game-home-team game) (game-away-team game)
						(game-home-goals game) (game-away-goals game)
						(+ (game-home-goals game) (game-away-goals game))))
			(safe-sort my-list #'< :key #'expect-goals)))
  t)
  
 (defun over-expects (&optional (goals 3))
  (let ((my-list nil))
	(mapcar #'(lambda (game)
				(when (> (expect-goals game)
						 goals)
				  (push game my-list)))
			*expects*)

	(mapcar #'(lambda (game)
				(format t "~% ~3a : ~a v ~a ~37tHome : ~,2f Away : ~,2f - ~,2f"
						(string-upcase (game-league game))
						(game-home-team game) (game-away-team game)
						(game-home-goals game) (game-away-goals game)
						(expect-goals game)))
			(safe-sort my-list #'> :key #'expect-goals)))
  t)