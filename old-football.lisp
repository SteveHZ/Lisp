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


sub calc {
	my ($self, $home_expect, $away_expect) = @_;
	state $p = Football::Game_Predictions::MyPoisson->new ();
	state $p_func = $p->get_calc_func ($self->{weighted});
	my %cache_p;

	for my $home_score (0..$self->{max}) {
		my $home_p = $p_func->($p, $home_expect, $home_score);
		for my $away_score (0..$self->{max}) {
			unless (exists $cache_p{$away_score}) {
				$cache_p{$away_score} = $p_func->($p, $away_expect, $away_score);
			}
			$self->{stats}[$home_score][$away_score] = $p->poisson_result ($home_p, $cache_p{$away_score});
		}
	}
	return $self->{stats};
}

sub poisson {
	my ($self, $expect, $score) = @_;
	return	power ($expect, $score) *
	 		power ($self->{euler}, $expect * -1) /
			factorial ($score);
}

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

