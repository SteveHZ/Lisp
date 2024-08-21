
;; football.lisp May 2020

(defpackage :football
  (:use :cl :cl-csv :iterate :parse-float :ppcre :local-time
   :my-odds :my-skellam :my-expectdb :data-table))

;;;
;;; Declare global variables and structures
;;;

(defvar *db*)
(defvar *teams*)
(defvar *expects*)
(defvar *expects-db*)
(defvar *sorted*)
(defvar *fixtures* nil)
(defvar *uk-csv-cols* '("Date" "HomeTeam" "AwayTeam" "FTHG" "FTAG" "FTR" "B365H" "B365D" "B365A" "B365>2.5" "B365<2.5"))
(defvar *summer-csv-cols* '("Date" "HomeTeam" "AwayTeam" "FTHG" "FTAG" "FTR" "AvgH" "AvgD" "AvgA")) ;; no over/under data
(defparameter *historical-csv-cols* '("Date" "HomeTeam" "AwayTeam" "FTHG" "FTAG" "FTR" "B365H" "B365D" "B365A" "BbAv>2.5" "BbAv<2.5"))
(defparameter *historical-csv-cols-2019+* '("Date" "HomeTeam" "AwayTeam" "FTHG" "FTAG" "FTR" "B365H" "B365D" "B365A" "B365>2.5" "B365<2.5"))
(defparameter *rugby-csv-cols* '("Date" "HomeTeam" "AwayTeam" "FTHG" "FTAG")) 

(defparameter *uk-leagues*
  '(("e0" "Premier League")
    ("e1" "Championship")
    ("e2" "League One")
    ("e3" "League Two")
    ("ec" "Conference")
	
    ("sc0" "Scots Premier")
    ("sc1" "Scots Championship")
    ("sc2" "Scots League One")
    ("sc3" "Scots League Two")))

(defparameter *euro-leagues*
  '(("d1" "German League")
	("i1" "Italian League")
	("sp1" "Spanish League")
	("f1" "French League")))

(defparameter *summer-leagues*
  '(("swe" "Swedish League")
	("nor" "Norwegian League")
	("fin" "Finnish League")
	("roi" "Irish League")
;	("mls" "American League")
	))

(defparameter *rugby-leagues*
  '(("SL" "Super League")
	("NRL" "Aussie League")))

(proclaim '(inline csv-filename csv-league-name league-assoc))
(defun csv-filename (league) (first league))
(defun csv-league-name (league) (second league))
(defun league-assoc (key league) (second (assoc key league)))

(defvar *uk-fixtures-file* "c:/mine/perl/football/data/fixtures.csv")
(defvar *euro-fixtures-file* "c:/mine/perl/football/data/euro/fixtures.csv")
(defvar *summer-fixtures-file* "c:/mine/perl/football/data/summer/fixtures.csv")
(defvar *summer-fixtures2-file* "c:/mine/perl/football/data/summer/fixtures2.csv")
(defvar *rugby-fixtures-file* "c:/mine/lisp/data/rugby-fixtures.csv")

(defvar *uk-teams-file* "c:/mine/lisp/data/uk-teams.dat")
(defvar *euro-teams-file* "c:/mine/lisp/data/euro-teams.dat")
(defvar *summer-teams-file* "c:/mine/lisp/data/summer-teams.dat")
(defvar *rugby-teams-file* "c:/mine/lisp/data/rugby-teams.dat")

(defparameter *leagues* *uk-leagues*)
(defparameter *fixtures-file* *uk-fixtures-file*)
(defparameter *csv-cols* *uk-csv-cols*)
(defparameter *teams-file* *uk-teams-file*)

(defparameter *leagues* *summer-leagues*)
(defparameter *fixtures-file* *summer-fixtures-file*)
(defparameter *csv-cols* *summer-csv-cols*)
(defparameter *teams-file* *summer-teams-file*)
(defparameter *leagues* (append *uk-leagues* *summer-leagues*))

(defvar *ht-stats* (make-hash-table :test #'equal))
(defvar *ht-league-stats* (make-hash-table :test #'equal))
(defvar *expects-db* (make-hash-table :test #'equal))
(defvar *my-teams*)
(defparameter *streak-teams* '())
(defparameter *summer-over-under-odds* 1.61) ;; work-around

(defun clear-database ()
  (setf *ht-stats* (make-hash-table :test #'equal))
  (setf *ht-league-stats* (make-hash-table :test #'equal))
  (setf *expects-db* (make-hash-table :test #'equal)))

(defstruct (stats (:print-function print-stats))
  "Holds the total home/away games/goals for/goals against for each team"
  (home-for 0) (av-home-for 0) (expect-home-for 0)
  (home-ag 0)  (av-home-ag 0)  (expect-home-ag 0)
  (away-for 0) (av-away-for 0) (expect-away-for 0)
  (away-ag 0)  (av-away-ag 0)  (expect-away-ag 0)
  (home-games 0) (away-games 0))

(defstruct (league-stats (:print-function print-league-stats))
  "Holds the total games and home/away goals for each league"
  (home-goals 0) (away-goals 0) (games 0) (av-home-goals 0) (av-away-goals 0))

(defstruct (game (:print-function print-game))
  "Holds the goal expectancy values for each current game"
  league home-team away-team home-goals away-goals goal-diff home-odds draw-odds away-odds over-odds under-odds)

(defun print-stats (x stream depth)
  (declare (ignore depth))
  (format stream "hf : ~2d ha : ~2d af : ~2d aa : ~2d Av hf : ~,2f  Av ha : ~,2f Av af : ~,2f Av aa : ~,2f xhf : ~,2f  xha : ~,2f xaf : ~,2f xaa : ~,2f"
          (stats-home-for x) (stats-home-ag x)
          (stats-away-for x) (stats-away-ag x)
          (stats-av-home-for x) (stats-av-home-ag x)
          (stats-av-away-for x) (stats-av-away-ag x)
          (stats-expect-home-for x) (stats-expect-home-ag x)
          (stats-expect-away-for x) (stats-expect-away-ag x)
		  ;;          (stats-home-games x) (stats-away-games x)
          ))

(defun print-league-stats (x stream depth)
  (declare (ignore depth))
  (format stream "home : ~d away : ~d games : ~d"
          (league-stats-home-goals x) (league-stats-away-goals x) (league-stats-games x)))

(defun print-game (x stream depth)
  (declare (ignore depth))
  (format stream " ~a : ~a v ~a ~40t~,2f  ~,2f  ~5,2f"
          (game-league x)
          (game-home-team x) (game-away-team x)
          (game-home-goals x) (game-away-goals x)
          (game-goal-diff x)))

(defun print-game-odds (game-list)
  (dolist (x game-list)
    (format t "~% ~3a : ~a v ~a ~42t~6,2f  ~6,2f  ~6,2f   | ~6,2f  ~6,2f"
            (string-upcase (game-league x))
            (game-home-team x) (game-away-team x)
            (game-home-odds x) (game-draw-odds x) (game-away-odds x)
			(game-over-odds x) (game-under-odds x))))

(defun get-league-odds (league)
  (let ((my-list nil))
	(mapcar #'(lambda (game)
				(when (string-equal (game-league game) league)
				  (push game my-list)))
			*expects*)
	my-list))

(defun league-odds (league &key (gtlt-fn #'>)
								(cmp-fn #'game-over-odds))
  (print-game-odds (sort (get-league-odds league) gtlt-fn :key cmp-fn)))

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

(defun is-home (team game) (equal team (home-team game)))
(defun is-away (team game) (equal team (away-team game)))
(defun home-away (team game)
  (or (is-home team game)
      (is-away team game)))

(defun home-away-win-result (team game)
  (or (and (is-home team game)
           (equal (result game) "H"))
      (and (is-away team game)
           (equal (result game) "A"))))

(defun home-away-defeat-result (team game)
  (or (and (is-home team game)
           (equal (result game) "A"))
      (and (is-away team game)
           (equal (result game) "H"))))

(defun not-home-away-win-result (team game)
  (not (home-away-win-result team game)))

(defun is-win (team game)
  (home-away-win-result team game))
(defun is-defeat (team game)
  (home-away-defeat-result team game))
(defun is-draw (game)
  (equal (result game) "D"))

(defun match-goals (game)
  (+ (home-score game)
	 (away-score game)))

(defun is-over (game &optional (n 2.5))
  (> (match-goals game) n))
(defun is-under (game &optional (n 2.5))
  (< (match-goals game) n))

(defun win-loss-result (team game)
  (or (and (is-home team game)
		   (equal (result game) "H"))
	  (and (is-away team game)
		   (equal (result game) "H"))))

(defun loss-win-result (team game)
  (or (and (is-home team game)
		   (equal (result game) "A"))
	  (and (is-away team game)
		   (equal (result game) "A"))))

;; Workarounds for do-series and do-streak
(defun series-odds (fn team game)
  (declare (ignore team))
  (funcall fn game))
(defun series-is-draw (team game)
  (declare (ignore team))
  (is-draw game))

(defun series-draw-odds (team game)
  (series-odds #'draw-odds team game))
(defun series-over-odds (team game)
  (series-odds #'over-odds team game))
(defun series-under-odds (team game)
  (series-odds #'under-odds team game))
(defun series-overs (team game)
  (series-odds #'is-over team game))
(defun series-unders (team game)
  (series-odds #'is-under team game))
(defun summer-ou-odds (team game)
  (declare (ignore team)
		   (ignore game))
  *summer-over-under-odds*)

(defun series-home-odds (team game)
  (declare (ignore team))
  (home-odds game))
(defun series-away-odds (team game)
  (declare (ignore team))
  (away-odds game))

(defun series-over-unders (team game)
  (if (string-equal team (home-team game))
	  (series-odds #'is-over team game)
	  (series-odds #'is-under team game)))
(defun series-under-overs (team game)
  (if (string-equal team (home-team game))
	  (series-odds #'is-under team game)
	  (series-odds #'is-over team game)))

(defun series-ou-odds (team game)
  (if (string-equal team (home-team game))
	  (series-odds #'over-odds team game)
	  (series-odds #'under-odds team game)))
(defun series-uo-odds (team game)
  (if (string-equal team (home-team game))
	  (series-odds #'under-odds team game)
	  (series-odds #'over-odds team game)))

(defun series-win-loss-odds (team game)
  (series-home-odds team game))
(defun series-loss-win-odds (team game)
  (series-away-odds team game))

;; Accessor functions for *fixtures*
(defun fdate-time (game) (first game))
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

(defmacro do-get-games (fn-name fn)
  `(defun ,fn-name (team)
	 (remove-if-not #'(lambda (game)
						(funcall ,fn team game))
					(get-league (find-league team)))))

(do-get-games homes #'is-home)
(do-get-games aways #'is-away)
(do-get-games home-aways #'home-away)

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

(defun home-away-odds (team game)
  (cond ((is-home team game)
		 (home-odds game))
		((is-away team game)
		 (away-odds game))
		(t 0)))

(defun home-away-lost-odds (team game)
  (cond ((is-home team game)
		 (away-odds game))
		((is-away team game)
		 (home-odds game))
		(t 0)))

(defun home-away-draw-result (team game)
  (and (home-away team game)
       (equal (result game) "D")))

(defun get-result (team game)
  (cond ((is-draw game) "D")
		((is-win team game) "W")
		(t "L")))

(defmacro do-get-results (fn-name games-fn)
  `(defun ,fn-name (team result-fn)
	 (remove-if-not #'(lambda (game)
						(funcall result-fn team game))
					(funcall ,games-fn team))))

(do-get-results with-homes #'homes)
(do-get-results with-aways #'aways)
(do-get-results with-all #'home-aways)

(defun home-wins (team)
  (with-homes team #'home-away-win-result))
(defun home-defeats (team)
  (with-homes team #'home-away-defeat-result))
(defun home-draws (team)
  (with-homes team #'home-away-draw-result))

(defun away-wins (team)
  (with-aways team #'home-away-win-result))
(defun away-defeats (team)
  (with-aways team #'home-away-defeat-result))
(defun away-draws (team)
  (with-aways team #'home-away-draw-result))

(defun wins (team)
  (with-all team #'home-away-win-result))
(defun defeats (team)
  (with-all team #'home-away-defeat-result))
(defun draws (team)
  (with-all team #'home-away-draw-result))

(defun home-overs (team)
  (with-homes team #'series-overs))
(defun home-unders (team)
  (with-homes team #'series-unders))

(defun away-overs (team)
  (with-aways team #'series-overs))
(defun away-unders (team)
  (with-aways team #'series-unders))

(defun overs (team)
  (with-all team #'series-overs))
(defun unders (team)
  (with-all team #'series-unders))

(defmacro do-get-last-six (fn-name games-fn)
  `(defun ,fn-name (team &optional (ngames 6))
	 (last-n (funcall ,games-fn team) ngames)))

(do-get-last-six last-six #'home-aways)
(do-get-last-six last-six-homes #'homes)
(do-get-last-six last-six-aways #'aways)

(do-get-last-six last-six-wins #'wins)
(do-get-last-six last-six-draws #'draws)
(do-get-last-six last-six-defeats #'defeats)

(do-get-last-six last-six-home-wins #'home-wins)
(do-get-last-six last-six-home-draws #'home-draws)
(do-get-last-six last-six-home-defeats #'home-defeats)

(do-get-last-six last-six-away-wins #'away-wins)
(do-get-last-six last-six-away-draws #'away-draws)
(do-get-last-six last-six-away-defeats #'away-defeats)

(do-get-results with-last-six #'last-six)
(do-get-results with-last-six-homes #'last-six-homes)
(do-get-results with-last-six-aways #'last-six-aways)

(defun wins-in-last-six (team)
  (with-last-six team #'home-away-win-result))
(defun defeats-in-last-six (team)
  (with-last-six team #'home-away-defeat-result))
(defun draws-in-last-six (team)
  (with-last-six team #'home-away-draw-result))

(defun wins-in-last-six-homes (team)
  (with-last-six-homes team #'home-away-win-result))
(defun defeats-in-last-six-homes (team)
  (with-last-six-homes team #'home-away-defeat-result))
(defun draws-in-last-six-homes (team)
  (with-last-six-homes team #'home-away-draw-result))

(defun wins-in-last-six-aways (team)
  (with-last-six-aways team #'home-away-win-result))
(defun defeats-in-last-six-aways (team)
  (with-last-six-aways team #'home-away-defeat-result))
(defun draws-in-last-six-aways (team)
  (with-last-six-aways team #'home-away-draw-result))

(defun last-n-homes (team n)
  (last-six-homes team n))
(defun last-n-aways (team n)
  (last-six-aways team n))
(defun last-n-games (team n)
  (last-six team n))

(defmacro do-get-n-results (fn-name games-fn)
  `(defun ,fn-name (team result-fn &optional (ngames 6))
	 (remove-if-not #'(lambda (game)
						(funcall result-fn team game))
					(funcall ,games-fn team ngames))))

(do-get-n-results with-last-n #'last-n-games)
(do-get-n-results with-last-n-homes #'last-n-homes)
(do-get-n-results with-last-n-aways #'last-n-aways)

(defun wins-in-last-n (team &optional (ngames 6))
  (with-last-n team #'home-away-win-result ngames));
(defun defeats-in-last-n (team &optional (ngames 6))
  (with-last-n team #'home-away-defeat-result ngames))
(defun draws-in-last-n (team &optional (ngames 6))
  (with-last-n team #'home-away-draw-result ngames))

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
  (or (home-away-defeat-result team game)
	  (home-away-draw-result team game)))
(defun home-away-undefeats (team game)
  (or (home-away-win-result team game)
	  (home-away-draw-result team game)))
(defun home-away-undraws (team game)
  (not (home-away-draw-result team game)))

(defun unwins (team)
  (with-all team #'home-away-unwins))
(defun undefeats (team)
  (with-all team #'home-away-undefeats))
(defun undraws (team)
  (with-all team #'home-away-undraws))

(defmacro do-get-over-unders (fn-name cmp-fn games-fn)
  "Function-building macro that returns a list of games either over or under (specified by CMP-FN)
   N goals for TEAM from a list of home/away/both games specified by GAMES-FN"
  `(defun ,fn-name (team &optional (n 2.5))
	 (remove-if-not #'(lambda (game)
						(funcall ,cmp-fn (match-goals game) n))
					(funcall ,games-fn team))))

(do-get-over-unders home-away-overs #'> #'home-aways)
(do-get-over-unders home-overs #'> #'homes)
(do-get-over-unders away-overs #'> #'aways)

(do-get-over-unders home-away-unders #'< #'home-aways)
(do-get-over-unders home-unders #'< #'homes)
(do-get-over-unders away-unders #'< #'aways)

(do-get-over-unders last-six-overs #'> #'last-six)
(do-get-over-unders last-six-home-overs #'> #'last-six-homes)
(do-get-over-unders last-six-away-overs #'> #'last-six-aways)

(do-get-over-unders last-six-unders #'< #'last-six)
(do-get-over-unders last-six-home-unders #'< #'last-six-homes)
(do-get-over-unders last-six-away-unders #'< #'last-six-aways)

;; ***************************************************************

;; DSL
;; Over Unders

(defun is-home-over (team game)
  (and (is-home team game)
	   (is-over game)))
(defun is-home-under (team game)
  (and (is-home team game)
	   (is-under game)))
(defun is-away-over (team game)
  (and (is-away team game)
	   (is-over game)))
(defun is-away-under (team game)
  (and (is-away team game)
	   (is-under game)))

(defun over-under-win-result (team game)
  (cond ((is-home-over team game)
		 (over-odds game))
		((is-away-under team game)
		 (under-odds game))
		(t 0)))

(defun under-over-win-result (team game)
  (cond ((is-home-under team game)
		 (under-odds game))
		((is-away-over team game)
		 (over-odds game))
		(t 0)))

;; end DSL

(defun get-results-from-list (result list)
  (remove-if-not #'(lambda (game)
					 (equal (result game) result))
				 list))

(defun get-league-home-wins (games-list)
  (get-results-from-list "H" games-list))
(defun get-league-away-wins (games-list)
  (get-results-from-list "A" games-list))
(defun get-league-draws (games-list)
  (get-results-from-list "D" games-list))

(defmacro do-last-n-over-unders (fn-name cmp-fn games-fn)
  "Function-building macro that returns a list of games either over or under (specified by CMP-FN)
   GOALS goals for TEAM from a list of last GAMES played by home/away/both games specified by GAMES-FN"
  `(defun ,fn-name (team &key (games 6) (goals 2.5))
	 (remove-if-not #'(lambda (game)
						(funcall ,cmp-fn (match-goals game) goals))
					(funcall ,games-fn team games))))

(do-last-n-over-unders last-n-overs #'> #'last-n-games)
(do-last-n-over-unders last-n-home-overs #'> #'last-n-homes)
(do-last-n-over-unders last-n-away-overs #'> #'last-n-aways)

(do-last-n-over-unders last-n-unders #'< #'last-n-games)
(do-last-n-over-unders last-n-home-unders #'< #'last-n-homes)
(do-last-n-over-unders last-n-away-unders #'< #'last-n-aways)

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

;; ***************************************************************

;; DSL
;; Output lists of games in readable form

(defun say-game (game)
  (if (equal *leagues* *summer-leagues*)
	  (format t "~%~{~a ~10t~a ~30t v  ~a ~54t~a-~a  ~a ~3*~}" game)    ; summer
	  (format t "~%~{~a ~10t~a ~30t v  ~a ~54t~a-~a  ~a ~5*~}" game)))  ; uk and euro

(defun say-game-with-odds (game)
  (if (equal *leagues* *summer-leagues*)
	  (format t "~%~{~a ~10t~a ~30t v  ~a ~54t~a-~a  ~a ~62t ¦  ~a ~73t~a ~79t~a ~85t ¦~}" game) ; summer
	  (format t "~%~{~a ~10t~a ~30t v  ~a ~54t~a-~a  ~a ~62t ¦  ~a ~73t~a ~79t~a ~85t ¦  ~a ~97t~a ~103t ¦~}" game))) ; uk and euro 

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

(defun over-under-results (team game)
  (if (is-home team game)
	  (splice-result-into-list team game #'over-result)
	  (splice-result-into-list team game #'under-result)))

(defun under-over-results (team game)
  (if (is-home team game)
	  (splice-result-into-list team game #'under-result)
	  (splice-result-into-list team game #'over-result)))

(defun result-list (team game)
  "Amend list to transform result column from [H A D] to [W L D] or [W L] for the given TEAM and possible result"
  (if (is-draw game)
	  game
	  (splice-result-into-list team game #'win-lose-result)))

(defun say (team games &key (odds t) (result-fn #'result-list))
  (let? odds-fn (equal odds nil)
	  #'say-game
	  #'say-game-with-odds
	
	(when (equal odds t)
	  (if (equal *leagues* *summer-leagues*)
		  (format t "~66t1~73tX~79t2")
		  (format t "~66t1~73tX~79t2~89tO~97tU")))
	(mapcar #'(lambda (game)
				(funcall odds-fn (funcall result-fn team game)))
			games))
  t)

;; Macro used to create a function named FN-NAME
;; which will return a list of games from GAMES-FN to output using SAY

(defmacro do-say (fn-name games-fn)
  `(defun ,fn-name (team &key (odds t))
	 (say team (funcall ,games-fn team) :odds odds)))

(do-say say-homes #'homes)
(do-say say-aways #'aways)
(do-say say-home-aways #'home-aways)

(do-say say-wins #'wins)
(do-say say-defeats #'defeats)
(do-say say-draws #'draws)

(do-say say-home-wins #'home-wins)
(do-say say-home-defeats #'home-defeats)
(do-say say-home-draws #'home-draws)

(do-say say-away-wins #'away-wins)
(do-say say-away-defeats #'away-defeats)
(do-say say-away-draws #'away-draws)

(do-say say-wins-in-last-six #'wins-in-last-six)
(do-say say-defeats-in-last-six #'defeats-in-last-six)
(do-say say-draws-in-last-six #'draws-in-last-six)

(do-say say-wins-in-last-six-homes #'wins-in-last-six-homes)
(do-say say-defeats-in-last-six-homes #'defeats-in-last-six-homes)
(do-say say-draws-in-last-six-homes #'draws-in-last-six-homes)

(do-say say-wins-in-last-six-aways #'wins-in-last-six-aways)
(do-say say-defeats-in-last-six-aways #'defeats-in-last-six-aways)
(do-say say-draws-in-last-six-aways #'draws-in-last-six-aways)

(do-say say-home-over-wins #'home-overs)
(do-say say-away-over-wins #'away-overs)
(do-say say-home-away-over-wins #'home-away-overs)

(do-say say-home-under-wins #'home-unders)
(do-say say-away-under-wins #'away-unders)
(do-say say-home-away-under-wins #'home-away-unders)

;; As with do-say but with optional number of recent games

(defmacro do-sayn (fn-name games-fn)
  `(defun ,fn-name (team &key (odds t) (ngames 6))
	 (say team (funcall ,games-fn team ngames) :odds odds)))

(do-sayn say-last-six #'last-six)
(do-sayn say-last-six-homes #'last-six-homes)
(do-sayn say-last-six-aways #'last-six-aways) 

(do-sayn say-last-six-wins #'last-six-wins)
(do-sayn say-last-six-defeats #'last-six-defeats)
(do-sayn say-last-six-draws #'last-six-draws)

(do-say say-last-six-over-wins #'last-six-overs)
(do-say say-last-six-home-over-wins #'last-six-home-overs)
(do-say say-last-six-away-over-wins #'last-six-away-overs)

(do-say say-last-six-under-wins #'last-six-unders)
(do-say say-last-six-home-under-wins #'last-six-home-unders)
(do-say say-last-six-away-under-wins #'last-six-unders)

;; As with do-say but showing all games with over/under results

(defmacro do-say-ou (fn-name games-fn result-fn)
  `(defun ,fn-name (team &key (odds t) (result-fn ,result-fn))
	 (say team (funcall ,games-fn team) :odds odds :result-fn result-fn)))

(do-say-ou say-home-overs #'homes #'over-result-list)
(do-say-ou say-away-overs #'aways #'over-result-list)
(do-say-ou say-home-away-overs #'home-aways #'over-result-list)

(do-say-ou say-home-unders #'homes #'under-result-list)
(do-say-ou say-away-unders #'aways #'under-result-list)
(do-say-ou say-home-away-unders #'home-aways #'under-result-list)

(do-say-ou say-over-unders #'home-aways #'over-under-results)
(do-say-ou say-under-overs #'home-aways #'under-over-results)

(do-say-ou say-last-six-overs #'last-six #'over-result-list)
(do-say-ou say-last-six-home-overs #'last-six-homes #'over-result-list)
(do-say-ou say-last-six-away-overs #'last-six-aways #'over-result-list)

(do-say-ou say-last-six-unders #'last-six #'under-result-list)
(do-say-ou say-last-six-home-unders #'last-six-homes #'under-result-list)
(do-say-ou say-last-six-away-unders #'last-six-aways #'under-result-list)

(defmacro do-say-n-ou (fn-name games-fn)
  `(defun ,fn-name (team &key (odds t) (games 6) (goals 2.5))
	 (say team (funcall ,games-fn team :games games :goals goals) :odds odds)))

(do-say-n-ou say-last-n-over-wins #'last-n-overs)
(do-say-n-ou say-last-n-home-over-wins #'last-n-home-overs)
(do-say-n-ou say-last-n-away-over-wins #'last-n-away-overs)

(do-say-n-ou say-last-n-under-wins #'last-n-unders)
(do-say-n-ou say-last-n-home-under-wins #'last-n-home-unders)
(do-say-n-ou say-last-n-away-under-wins #'last-n-away-unders)

(defun get-team-ou-stats (team games-fn ou-fn)
  (labels ((inner (wins-count games-count)
			 (mapcar #'(lambda (game)
						 (incf games-count)
						 (when (funcall ou-fn game)
						   (incf wins-count)))
					 (funcall games-fn team))
			 (values wins-count games-count)))
	(inner 0 0)))

(defun get-team-over-stats (team games-fn)
  (get-team-ou-stats team games-fn #'is-over))
(defun get-team-under-stats (team games-fn)
  (get-team-ou-stats team games-fn #'is-under))

(defun say-game-over-stats (home-team away-team)
  (multiple-value-bind (overs games) (get-team-over-stats home-team #'last-six-homes)
	(format t "~%Home Team : ~a : ~a from ~a = ~,2f %~%" home-team overs games (calc-percent games overs))
	(say-last-six-homes home-team))
  (multiple-value-bind (overs games) (get-team-over-stats away-team #'last-six-aways)
	(format t "~%~%Away Team : ~a : ~a from ~a = ~,2f %~%" away-team overs games (calc-percent games overs))
	(say-last-six-aways away-team)))

(defun say-game-under-stats (home-team away-team)
  (multiple-value-bind (unders games) (get-team-under-stats home-team #'last-six-homes)
	(format t "~%Home Team : ~a : ~a from ~a = ~,2f %~%" home-team unders games (calc-percent games unders))
	(say-last-six-homes home-team))
  (multiple-value-bind (unders games) (get-team-under-stats away-team #'last-six-aways)
	(format t "~%~%Away Team : ~a : ~a from ~a = ~,2f %~%" away-team unders games (calc-percent games unders))
	(say-last-six-aways away-team)))

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

(defun percentage-return (games-fn results-fn odds-fn team)
  (let ((ngames (length (funcall games-fn team))))
	(cond ((zerop ngames)
		   (values 0 0 0))
		  (t (let ((team-return (returns odds-fn (funcall results-fn team))))
			   (values (/ team-return ngames)
					   team-return
					   ngames))))))

(defun ha-percentage-return (games-fn results-fn odds-fn team)
  (let ((ngames (length (funcall games-fn team))))
	(cond ((zerop ngames)
		   (values 0 0 0))
		  (t (let ((team-return (ha-returns team odds-fn (funcall results-fn team))))
			   (values (/ team-return ngames)
					   team-return
					   ngames))))))

;; ***************************************************************
;; Calculate returns for each team - required for say-return-stats

(defun home-win-returns (team)
  (returns #'home-odds (home-wins team)))
(defun away-win-returns (team)
  (returns #'away-odds (away-wins team)))

(defun home-loss-returns (team)
  (returns #'away-odds (home-defeats team)))
(defun away-loss-returns (team)
  (returns #'home-odds (away-defeats team)))

(defun win-returns (team)
  (+ (home-win-returns team)
     (away-win-returns team)))
(defun loss-returns (team)
  (+ (home-loss-returns team)
     (away-loss-returns team)))

(defun draw-returns (team)
  (returns #'draw-odds (draws team)))
(defun home-draw-returns (team)
  (returns #'draw-odds (home-draws team)))
(defun away-draw-returns (team)
  (returns #'draw-odds (away-draws team)))

(defun home-away-over-returns (team)
  (returns #'over-odds (home-away-overs team)))
(defun home-over-returns (team)
  (returns #'over-odds (home-overs team)))
(defun away-over-returns (team)
  (returns #'over-odds (away-overs team)))

(defun home-away-under-returns (team)
  (returns #'under-odds (home-away-unders team)))
(defun home-under-returns (team)
  (returns #'under-odds (home-unders team)))
(defun away-under-returns (team)
  (returns #'under-odds (away-unders team)))

(defun last-six-win-returns (team)
  (ha-returns team #'home-away-odds (wins-in-last-six team)))
(defun last-six-homes-return (team)
  (returns #'home-odds (wins-in-last-six-homes team)))
(defun last-six-aways-return (team)
  (returns #'away-odds (wins-in-last-six-aways team)))

(defun last-six-loss-returns (team)
  (ha-returns team #'home-away-lost-odds (defeats-in-last-six team)))
(defun last-six-home-loss-returns (team)
  (returns #'away-odds (defeats-in-last-six-homes team)))
(defun last-six-away-loss-returns (team)
  (returns #'home-odds (defeats-in-last-six-aways team)))

(defun last-six-draw-return (team)
  (returns #'draw-odds (draws-in-last-six team)))
(defun last-six-home-draw-return (team)
  (returns #'draw-odds (draws-in-last-six-homes team)))
(defun last-six-away-draw-return (team)
  (returns #'draw-odds (draws-in-last-six-aways team)))

(defun last-six-over-returns (team)
  (returns #'over-odds (last-six-overs team)))
(defun last-six-home-over-returns (team)
  (returns #'over-odds (last-six-home-overs team)))
(defun last-six-away-over-returns (team)
  (returns #'over-odds (last-six-away-overs team)))

(defun last-six-under-returns (team)
  (returns #'under-odds (last-six-unders team)))
(defun last-six-home-under-returns (team)
  (returns #'under-odds (last-six-home-unders team)))
(defun last-six-away-under-returns (team)
  (returns #'under-odds (last-six-away-unders team)))

(defun over-under-returns (team)
  (+ (home-over-returns team)
	 (away-under-returns team)))
(defun under-over-returns (team)
  (+ (home-under-returns team)
	 (away-over-returns team)))

(defun win-loss-returns (team)
  (+ (home-win-returns team)
	 (away-loss-returns team)))
(defun loss-win-returns (team)
  (+ (home-loss-returns team)
	 (away-win-returns team)))

;; ***************************************************************
;; Calculate percentage returns

(defun win-percentage-return (team)
  (ha-percentage-return #'home-aways #'wins #'home-away-odds team))
(defun home-win-percentage-return (team)
  (percentage-return #'homes #'home-wins #'home-odds team))
(defun away-win-percentage-return (team)
  (percentage-return #'aways #'away-wins #'away-odds team))

(defun loss-percentage-return (team)
  (ha-percentage-return #'home-aways #'defeats #'home-away-lost-odds team))
(defun home-loss-percentage-return (team)
  (percentage-return #'homes #'home-defeats #'away-odds team))
(defun away-loss-percentage-return (team)
  (percentage-return #'aways #'away-defeats #'home-odds team))

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

;; ***************************************************************
;; Show all teams in given LEAGUE for each predicate

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

(defmacro do-league% (fn-name returns-fn)
  `(defun ,fn-name (league)
	 (percents-table
	  (safe-sort (league-percents ,returns-fn league) #'> :key #'fifth))))

(do-league% do-win-percents #'win-percentage-return)
(do-league% do-home-win-percents #'home-win-percentage-return)
(do-league% do-away-win-percents #'away-win-percentage-return)

(do-league% do-loss-percents #'loss-percentage-return)
(do-league% do-home-loss-percents #'home-loss-percentage-return)
(do-league% do-away-loss-percents #'away-loss-percentage-return)

(do-league% do-draw-percents #'draw-percentage-return)
(do-league% do-home-draw-percents #'home-draw-percentage-return)
(do-league% do-away-draw-percents #'away-draw-percentage-return)

(do-league% do-over-percents #'over-percentage-return)
(do-league% do-home-over-percents #'home-over-percentage-return)
(do-league% do-away-over-percents #'away-over-percentage-return)

(do-league% do-under-percents #'under-percentage-return)
(do-league% do-home-under-percents #'home-under-percentage-return)
(do-league% do-away-under-percents #'away-under-percentage-return)

(do-league% do-last-six-win-percents #'last-six-win-percentage-return)
(do-league% do-last-six-home-win-percents #'last-six-home-win-percentage-return)
(do-league% do-last-six-away-win-percents #'last-six-away-win-percentage-return)

(do-league% do-last-six-loss-percents #'last-six-win-percentage-return)
(do-league% do-last-six-home-loss-percents #'last-six-home-loss-percentage-return)
(do-league% do-last-six-away-loss-percents #'last-six-away-loss-percentage-return)

(do-league% do-last-six-draw-percents #'last-six-draw-percentage-return)
(do-league% do-last-six-home-draw-percents #'last-six-home-draw-percentage-return)
(do-league% do-last-six-away-draw-percents #'last-six-away-draw-percentage-return)

(do-league% last-six-over-percents #'last-six-over-percentage-return)
(do-league% last-six-home-over-percents #'last-six-home-over-percentage-return)
(do-league% last-six-away-over-percents #'last-six-away-over-percentage-return)

(do-league% last-six-under-percents #'last-six-under-percentage-return)
(do-league% last-six-home-under-percents #'last-six-home-under-percentage-return)
(do-league% last-six-away-under-percents #'last-six-away-under-percentage-return)

;; ***************************************************************
;; Show all teams in all leagues for each predicate

(defun percents-all (fn)
  "Calculate percentage returns for all teams in all leagues"
  (let ((my-list nil))
    (dolist (league *leagues*)
      (dolist (team (league-percents fn (csv-filename league)))
        (push team my-list)))
    my-list))

(defmacro do-all% (fn-name returns-fn)
  `(defun ,fn-name ()
	 (percents-table
	  (safe-sort (percents-all ,returns-fn) #'< :key #'fifth))))

(do-all% do-win-percents-all #'win-percentage-return)
(do-all% do-home-win-percents-all #'home-win-percentage-return)
(do-all% do-away-win-percents-all #'away-win-percentage-return)

(do-all% do-loss-percents-all #'loss-percentage-return)
(do-all% do-home-loss-percents-all #'home-loss-percentage-return)
(do-all% do-away-loss-percents-all #'away-loss-percentage-return)

(do-all% do-draw-percents-all #'draw-percentage-return)
(do-all% do-home-draw-percents-all #'home-draw-percentage-return)
(do-all% do-away-draw-percents-all #'away-draw-percentage-return)

(do-all% do-over-percents-all #'over-percentage-return)
(do-all% do-home-over-percents-all #'home-over-percentage-return)
(do-all% do-away-over-percents-all #'away-over-percentage-return)

(do-all% do-under-percents-all #'under-percentage-return)
(do-all% do-home-under-percents-all #'home-under-percentage-return)
(do-all% do-away-under-percents-all #'away-under-percentage-return)

(do-all% do-last-six-win-percents-all #'last-six-win-percentage-return)
(do-all% do-last-six-home-win-percents-all #'last-six-home-win-percentage-return)
(do-all% do-last-six-away-win-percents-all #'last-six-away-win-percentage-return)

(do-all% do-last-six-draw-percents-all #'last-six-draw-percentage-return)
(do-all% do-last-six-home-draw-percents-all #'last-six-home-draw-percentage-return)
(do-all% do-last-six-away-draw-percents-all #'last-six-away-draw-percentage-return)

(do-all% do-last-six-loss-percents-all #'last-six-loss-percentage-return)
(do-all% do-last-six-home-loss-percents-all #'last-six-home-loss-percentage-return)
(do-all% do-last-six-away-loss-percents-all #'last-six-away-loss-percentage-return)

(do-all% do-last-six-over-percents-all #'last-six-over-percentage-return)
(do-all% do-last-six-home-over-percents-all #'last-six-home-over-percentage-return)
(do-all% do-last-six-away-over-percents-all #'last-six-away-over-percentage-return)

(do-all% do-last-six-under-percents-all #'last-six-under-percentage-return)
(do-all% do-last-six-home-under-percents-all #'last-six-home-under-percentage-return)
(do-all% do-last-six-away-under-percents-all #'last-six-away-under-percentage-return)

;; ***************************************************************
;; Show only top n teams in all leagues for each predicate

(defun do-top%-list (returns-fn n)
  (first-n n (safe-sort (percents-all returns-fn)
			   #'> :key #'fifth)))

(defmacro do-top% (fn-name returns-fn)
  `(defun ,fn-name (&optional (n 10))
	 (percents-table (do-top%-list ,returns-fn n))))

(do-top% top-win-percents #'win-percentage-return)
(do-top% top-home-win-percents #'home-win-percentage-return)
(do-top% top-away-win-percents #'away-win-percentage-return)

(do-top% top-loss-percents #'loss-percentage-return)
(do-top% top-home-loss-percents #'home-loss-percentage-return)
(do-top% top-away-loss-percents #'away-loss-percentage-return)

(do-top% top-draw-percents #'draw-percentage-return)
(do-top% top-home-draw-percents #'home-draw-percentage-return)
(do-top% top-away-draw-percents #'away-draw-percentage-return)

(do-top% top-over-percents #'over-percentage-return)
(do-top% top-home-over-percents #'home-over-percentage-return)
(do-top% top-away-over-percents #'away-over-percentage-return)

(do-top% top-under-percents #'under-percentage-return) 
(do-top% top-home-under-percents #'home-under-percentage-return) 
(do-top% top-away-under-percents #'away-under-percentage-return) 

(do-top% top-last-six-win-percents #'last-six-win-percentage-return)
(do-top% top-last-six-home-win-percents #'last-six-home-win-percentage-return)
(do-top% top-last-six-away-win-percents #'last-six-away-win-percentage-return)

(do-top% top-last-six-loss-percents #'last-six-loss-percentage-return)
(do-top% top-last-six-home-loss-percents #'last-six-home-loss-percentage-return)
(do-top% top-last-six-away-loss-percents #'last-six-away-loss-percentage-return)

(do-top% top-last-six-draw-percents #'last-six-draw-percentage-return)
(do-top% top-last-six-home-draw-percents #'last-six-home-draw-percentage-return)
(do-top% top-last-six-away-draw-percents #'last-six-away-draw-percentage-return)

(do-top% top-last-six-over-percents #'last-six-over-percentage-return)
(do-top% top-last-six-home-over-percents #'last-six-home-over-percentage-return)
(do-top% top-last-six-away-over-percents #'last-six-away-over-percentage-return)

(do-top% top-last-six-under-percents #'last-six-under-percentage-return)
(do-top% top-last-six-home-under-percents #'last-six-home-under-percentage-return)
(do-top% top-last-six-away-under-percents #'last-six-away-under-percentage-return)

;; ***************************************************************
;; DSL
;; Recent stats
;;

(defun recent-percentage-return (games-fn odds-fn team games goals)
  (let ((games-list (funcall games-fn team :games games :goals goals)))
	(cond ((zerop games)
		   (values 0 0 0))
		  (t (let ((team-return (returns odds-fn games-list)))
			   (values (/ team-return games)
					   team-return
					   games))))))

(defun recent-over-percentage-return (team games goals)
  (recent-percentage-return #'last-n-overs #'over-odds team games goals))
(defun recent-home-over-percentage-return (team games goals)
  (recent-percentage-return #'last-n-home-overs #'over-odds team games goals))
(defun recent-away-over-percentage-return (team games goals)
  (recent-percentage-return #'last-n-away-overs #'over-odds team games goals))

(defun recent-under-percentage-return (team games goals)
  (recent-percentage-return #'last-n-unders #'under-odds team games goals))
(defun recent-home-under-percentage-return (team games goals)
  (recent-percentage-return #'last-n-home-unders #'under-odds team games goals))
(defun recent-away-under-percentage-return (team games goals)
  (recent-percentage-return #'last-n-away-unders #'under-odds team games goals))

(defun recent-league-percents (fn csv-league games goals)
  "Calculate (loss) percentage returns for each TEAM in LEAGUE"
  (mapcar #'(lambda (team)
			  (multiple-value-bind (percent team-return ngames) (funcall fn team games goals)
				(list (string-upcase csv-league)
					  team
					  ngames
					  (my-round team-return 0.01)
					  (my-round percent 0.01))))
          (get-teams csv-league)))

(defun recent-percents-all (fn games goals)
  "Calculate percentage returns for all teams in all leagues"
  (let ((my-list nil))
    (dolist (league *leagues*)
      (dolist (team (recent-league-percents fn (csv-filename league) games goals))
        (push team my-list)))
    my-list))

(defmacro do-all-recent (fn-name returns-fn)
  `(defun ,fn-name (&key (games 6) (goals 2.5))
	 (percents-table
	  (safe-sort (recent-percents-all ,returns-fn games goals) #'< :key #'fifth))))

(do-all-recent do-recent-over-percents-all #'recent-over-percentage-return)
(do-all-recent do-recent-under-percents-all #'recent-under-percentage-return)

(defun do-top-list-recent (returns-fn n games goals)
  (first-n n (safe-sort (recent-percents-all returns-fn games goals)
			   #'> :key #'fifth)))

(defmacro do-top-recent (fn-name returns-fn)
  `(defun ,fn-name (&key (games 6) (goals 2.5) (n 10))
	 (percents-table (do-top-list-recent ,returns-fn n games goals))))

(do-top-recent top-recent-over-percents #'recent-over-percentage-return)
(do-top-recent top-recent-home-over-percents #'recent-home-over-percentage-return)
(do-top-recent top-recent-away-over-percents #'recent-away-over-percentage-return)

(do-top-recent top-recent-under-percents #'recent-under-percentage-return)
(do-top-recent top-recent-home-under-percents #'recent-home-under-percentage-return)
(do-top-recent top-recent-away-under-percents #'recent-away-under-percentage-return)

;; ***************************************************************
;; Returns stats

(defun print-header ()
  (format t "~11t |~16t Wins~26t | ~30t Losses ~42t| ~46t Draws ~64t Overs ~75t| ~79t Unders")
  (format t "~%--------------------------------------------------------~61t-----------------------------"))

(defmacro with-stats-format (text &body body)
  `(format t "~%~a~7t  ~2a | £~5,2f ~,2f% | £~5,2f ~,2f% | £~5,2f ~,2f% ~60t £~5,2f  ~,2f% | £~5,2f  ~,2f%"
		   ,text ,@body))

(defun get-home-away-stats-detail (team)
  (with-stats-format "All"
	(length (home-aways team))
	(win-returns team) (win-percentage-return team)
	(loss-returns team) (loss-percentage-return team)
	(draw-returns team) (draw-percentage-return team)
	(home-away-over-returns team) (over-percentage-return team)
	(home-away-under-returns team) (under-percentage-return team)))

(defun get-home-stats-detail (team)
  (with-stats-format "Homes"
	(length (homes team))
	(home-win-returns team) (home-win-percentage-return team)
	(home-loss-returns team) (home-loss-percentage-return team)
	(home-draw-returns team) (home-draw-percentage-return team)
	(home-over-returns team) (home-over-percentage-return team)
	(home-under-returns team) (home-under-percentage-return team)))

(defun get-away-stats-detail (team)
  (with-stats-format "Aways"
	(length (aways team))
	(away-win-returns team) (away-win-percentage-return team)
	(away-loss-returns team) (away-loss-percentage-return team)
	(away-draw-returns team) (away-draw-percentage-return team)
	(away-over-returns team) (away-over-percentage-return team)
	(away-under-returns team) (away-under-percentage-return team)))

(defun get-home-away-stats (team)
  (print-header)
  (get-home-away-stats-detail team))
(defun get-home-stats (team)
  (print-header)
  (get-home-stats-detail team))
(defun get-away-stats (team)
  (print-header)
  (get-away-stats-detail team))

(defun get-win-loss-stats (team)
  (let ((wl-returns (win-loss-returns team))
		(lw-returns (loss-win-returns team))
		(games (total-games team)))

	(format t "~%~%Win-Loss    : £~5,2f~5,2f%"
			wl-returns (/ wl-returns games))
	(format t "~%Loss-Win    : £~5,2f~5,2f%"
			lw-returns (/ lw-returns games))))

(defun get-ou-stats (team)
  (let ((ou-returns (over-under-returns team))
		(uo-returns (under-over-returns team))
		(games (total-games team)))

	(format t "~%~%Over-Unders : £~5,2f~5,2f%"
			ou-returns (/ ou-returns games ))
	(format t "~%Under-Overs : £~5,2f~5,2f%"
			uo-returns (/ uo-returns games))))

(defun say-return-stats (team)
  "Show return stats for given TEAM"
  (print-header)
  (get-home-away-stats-detail team)
  (get-home-stats-detail team)
  (get-away-stats-detail team)
  (get-win-loss-stats team)
  (get-ou-stats team))

;; ***************************************************************
;; Returns spreadsheet

(defparameter returns-funcs
  `(("Wins" ,#'win-percentage-return)
	("Home Wins" ,#'home-win-percentage-return)
	("Away Wins" ,#'away-win-percentage-return)

	("Draws" ,#'draw-percentage-return)
	("Home Draws" ,#'home-draw-percentage-return)
	("Away Draws" ,#'away-draw-percentage-return)

	("Defeats" ,#'loss-percentage-return)
	("Home Defeats" ,#'home-loss-percentage-return)
	("Away Defeats" ,#'away-loss-percentage-return)

	("Overs" ,#'over-percentage-return)
	("Home Overs" ,#'home-over-percentage-return)
	("Away Overs" ,#'away-over-percentage-return)	

	("Unders" ,#'under-percentage-return)
	("Home Unders" ,#'home-under-percentage-return)
	("Away Unders" ,#'away-under-percentage-return)

	("Last Six Wins" ,#'last-six-win-percentage-return)
	("Last Six Home Wins" ,#'last-six-home-win-percentage-return)
	("Last Six Away Wins" ,#'last-six-away-win-percentage-return)

	("Last Six Draws" ,#'last-six-draw-percentage-return)
	("Last Six Home Draws" ,#'last-six-home-draw-percentage-return)
	("Last Six Away Draws" ,#'last-six-away-draw-percentage-return)

	("Last Six Defeats" ,#'last-six-loss-percentage-return)
	("Last Six Home Defeats" ,#'last-six-home-loss-percentage-return)
	("Last Six Away Defeats" ,#'last-six-away-loss-percentage-return)

	("Last Six Overs" ,#'last-six-over-percentage-return)
	("Last Six Home Overs" ,#'last-six-home-over-percentage-return)
	("Last Six Away Overs" ,#'last-six-away-over-percentage-return)

	("Last Six Unders" ,#'last-six-under-percentage-return)
	("Last Six Home Unders" ,#'last-six-home-under-percentage-return)
	("Last Six Away Unders" ,#'last-six-away-under-percentage-return)))

(defun write-returns (filename return-fns n)
  (with-open-file (stream filename
						  :direction :output
						  :if-exists :supersede)
	(dolist (returns-pair return-fns)
	  (destructuring-bind (result return-fn) returns-pair
		(format t "~%Writing Returns - ~a..." result)
		(format stream "~a~%" result)
		(dolist (my-list (do-top%-list return-fn n))
		  (format stream "~{~a~^,~}~%" my-list)))
	  (format stream "~%"))))

(defun export-returns (&optional (n 50))
  (let ((filename "")
		(funcs returns-funcs))
	(cond ((equal *leagues* *uk-leagues*)
		   (setf filename "c:/mine/lisp/data/returns uk.csv"))
		  ((equal *leagues* *euro-leagues*)
		   (setf filename "c:/mine/lisp/data/returns euro.csv"))
		  (t (setf filename "c:/mine/lisp/data/returns summer.csv")))
	(write-returns filename funcs n)))

;; ***************************************************************
;; DSL
;; Accessor macros and functions for structures
;; Find stats within *ht-stats* hash for TEAM in LEAGUE
;;

;; Enables writing (get-home-for "Stoke" "e1)
;; rather than (stats-home-for (gethash "Stoke" (gethash "e1" *ht-stats*)))

(defmacro get-value (property team hash)
  `(,property (gethash ,team ,hash)))

(defun get-home-for (team league)
  (get-value stats-home-for team (gethash league *ht-stats*)))
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

(defun get-league-stats-home-goals (league)
  (get-value league-stats-home-goals league *ht-league-stats*))
(defun get-league-stats-away-goals (league)
  (get-value league-stats-away-goals league *ht-league-stats*))
(defun get-league-stats-av-home-goals (league)
  (get-value league-stats-av-home-goals league *ht-league-stats*))
(defun get-league-stats-av-away-goals (league)
  (get-value league-stats-av-away-goals league *ht-league-stats*))
(defun get-league-stats-games (league)
  (get-value league-stats-games league *ht-league-stats*))

(defun get-home-games (team hash)
  (get-value stats-home-games team hash))
(defun get-away-games (team hash)
  (get-value stats-away-games team hash ))

(defmacro set-value (property team hash value)
  "Set HASH PROPERTY to VALUE for TEAM"
  `(setf (get-value ,property ,team ,hash) ,value))

(defmacro incr-value (property team hash value)
  "Increase HASH PROPERTY by VALUE for TEAM"
  (let ((previous (gensym)))
    `(let ((,previous (get-value ,property ,team ,hash)))
       (setf (get-value ,property ,team ,hash) (+ ,previous ,value)))))

(defun set-home-for (team hash val)  (incr-value stats-home-for team hash val))
(defun set-home-ag  (team hash val)  (incr-value stats-home-ag team hash val))
(defun set-away-for (team hash val)  (incr-value stats-away-for team hash val))
(defun set-away-ag  (team hash val)  (incr-value stats-away-ag team hash val))

(defun set-av-home-for (team hash val)  (set-value stats-av-home-for team hash val))
(defun set-av-home-ag  (team hash val)  (set-value stats-av-home-ag team hash val))
(defun set-av-away-for (team hash val)  (set-value stats-av-away-for team hash val))
(defun set-av-away-ag  (team hash val)  (set-value stats-av-away-ag team hash val))

(defun set-expect-home-for (team hash val)  (set-value stats-expect-home-for team hash val))
(defun set-expect-home-ag  (team hash val)  (set-value stats-expect-home-ag team hash val))
(defun set-expect-away-for (team hash val)  (set-value stats-expect-away-for team hash val))
(defun set-expect-away-ag  (team hash val)  (set-value stats-expect-away-ag team hash val))

(defun set-league-stats-home-goals (league-name val) (incr-value league-stats-home-goals league-name *ht-league-stats* val))
(defun set-league-stats-away-goals (league-name val) (incr-value league-stats-away-goals league-name *ht-league-stats* val))
(defun set-league-stats-av-home-goals (league-name val) (set-value league-stats-av-home-goals league-name *ht-league-stats* val))
(defun set-league-stats-av-away-goals (league-name val) (set-value league-stats-av-away-goals league-name *ht-league-stats* val))

;; ***************************************************************
;; Utilities

(defun export-csv (data file)
  (with-open-file (stream file :direction :output
                               :if-exists :supersede)
    (cl-csv:write-csv data :stream stream)))

(defun import-csv (file)
  (with-open-file (stream file :external-format :utf-8)
    (cl-csv:read-csv stream)))

(defun import-teams ()
  (with-open-file (in *teams-file*)
    (setf *teams* (read in))))

(defun load-league-data (league)
  (list `(,(csv-filename league) .
		  ,(import-csv
			(format nil "c:/mine/lisp/data/~a.csv" (csv-filename league))))))

(defun load-leagues ()
  "Load all CSV files for leagues in *leagues*"
  (setf *db* nil)
  (dolist (league *leagues*)
    (setf *db* (append *db* (load-league-data league)))))

(defun load-rugby ()
  (import-csv "c:/mine/lisp/data/rugby2024.csv"))

(defun import-rugby-csv ()
  (setf *db* nil)
  (setf *db* (load-league-data (first *rugby-leagues*))))

(defun load-fixtures ()
  (setf *fixtures* (import-csv *fixtures-file*)))

(defun show-fixtures ()
  "Shows current fixtures file, will return NIL if not loaded"
  (when (null *fixtures*)
	(return-from show-fixtures nil))
  (dolist (game *fixtures*)
	(format t "~%~a : ~3a - ~a v ~a" (fdate-time game) (string-upcase (fleague game)) (fhome game) (faway game))))

(defun show-league-fixtures (league)
  "Shows current fixtures for LEAGUE, will return NIL if not loaded"
  (when (null *fixtures*)
	(return-from show-league-fixtures nil))
  (dolist (game *fixtures*)
	(when (string-equal league (fleague game))
	  (format t "~%~a : ~3a - ~a v ~a" (fdate-time game) (string-upcase (fleague game)) (fhome game) (faway game)))))

(defun get-team-stats (team)
  "Get stats for TEAM"
  (gethash team (gethash (find-league team) *ht-stats*)))

(defun show-hash (hash)
  "Prints out the given HASH"
  (maphash #'(lambda (k v)
               (format t "~%~a ~20t=> ~a" k v))
           hash))

(defun show-nested-hash (hash)
  "Prints out stats for the given nested HASH, such as *ht-stats*"
  (maphash #'(lambda (key value)
               (format t "~%~%League : ~a" (league-assoc key *leagues*))
               (show-hash value))
           hash))

(defun show-stats (csv-league)
  "Prints out stats for given CSV-LEAGUE"
  (show-hash (gethash csv-league *ht-stats*)))

(defun show-av-league-goals ()
  "Show average goals stats for each league"
  (format-table t (mapcar #'(lambda (league)
							  (let ((csv-file (csv-filename league)))
								(list (csv-league-name league)
									  (format nil "~,2f" (get-league-stats-av-home-goals csv-file))
									  (format nil "~,2f" (get-league-stats-av-away-goals csv-file)))))
						  *leagues*)
				:column-label '("League" "Home" "Away")
				:column-align '(:center :center :center)))

(defun load-my-teams ()
  (with-open-file (in "c:/mine/lisp/data/my-teams.dat")
	(with-standard-io-syntax
	  (setf *my-teams* (read in)))))

(defun save-my-teams ()
  (with-open-file (out "c:/mine/lisp/data/my-teams.dat"
					   :direction :output
					   :if-exists :supersede)
	(with-standard-io-syntax
	  (print *my-teams* out))))

(defun my-teams ()
  *my-teams*)

(defun my-teams-add (&rest team-list)
  (dolist (team team-list)
	(push team *my-teams*))
  (save-my-teams))

(defun my-teams-remove (&rest team-list)
  (dolist (team team-list)
	(setf *my-teams*
		  (remove-if #'(lambda (tm)
						 (string-equal tm team))
					 *my-teams*)))
  (save-my-teams))

(defun my-teams-remove-all ()
  (setf *my-teams* nil))

(defun my-teams-update-all (&rest team-list)
  (my-teams-remove-all)
  (apply #'my-teams-add team-list))

(defun my-fixtures ()
  (format-table 
   t (remove-if-not #'(lambda (game)
						(or (member (fhome game) *my-teams* :test #'equal)
							(member (faway game) *my-teams* :test #'equal)))
					*fixtures*)
   :column-label '("Date" "League" "Home" "Away")
   :column-align '(:center :center :center :center)))

(defun list-games (teams)
  (mapcar #'(lambda (team)
			  (format t "~%~%~a :~%" team)
			  (say-last-six team :odds t))
		  teams))

;; ***************************************************************
;; Odds utilities
;; Convert odds between different formats
;;

(defun decimal-to-percent (odds)
  (* 100 (/ 1.0 odds)))

(defun percent-to-decimal (pc)
  (/ 100.0 pc))

(defun frac-to-decimal (str-frac)
  (let ((nums (ppcre:split "-" str-frac)))
	(+ 1.0 (/ (parse-integer (first nums))
			  (parse-integer (second nums))))))

(defun frac-to-percent (frac)
  (decimal-to-percent
   (frac-to-decimal frac)))

(defun approx-equal (a b &optional (diff 0.1))
  "Helper function for decimal-to-frac to counter floating-point errors,
  amount of error allowed can be amended by DIFF"
  (if (<= (abs (- a b)) diff)
	  't 'nil))

(defun decimal-to-frac (dec)
;; Doesn't work for odds below evens
  (let ((count 1)
		(x (- dec 1)))
	(when (approx-equal 0.33 (mod (* x 1) 1))
	  (+= x 0.01)) ; work-around for 4.33 -> "10-3"
	(my-unless (approx-equal 0 (mod (* x count) 1))
	  (incf count))
	(format nil "~d-~d" (floor (* x count)) count)))

(defun percent-to-frac (pc)
  (decimal-to-frac
   (percent-to-decimal pc)))

(defun moneyline-decimal (odds)
  (cond ((> odds 0)
		 (format nil "~,2f" (1+ (/ odds 100))))
		((< odds 0)
		 (format nil "~,2f" (1+ (/ 100 (* -1 odds)))))
		(t 0)))

(defun decimal-moneyline (odds)
  (if (>= odds 2)
	  (format nil "~,2f" (* 100 (1- odds)))
	  (format nil "~,2f" (/ 100 (1- odds)))))

;; ***************************************************************
;; DSL Stake utilities
;;

(defun calc-new-stake-first (ds)
  "Calculate stake for system bet if odds are under 2.1"
  (let ((amount 1))
	(my-while (< (* amount ds) 2.1)
	  (+= amount 0.1))
	(format t "£~,2f" amount)))

(defun calc-new-stake-x (stake ds1 ds2)
  "Calculate stake for system (double) bet if odds are under 2.1"
  (let ((amount 1))
	(my-while (< (* amount ds1 ds2)
				 (* 2.1 2.1))
	  (+= amount 0.1))
	(format t "£~,2f" (* stake amount))))

(defun next-stake (total-stake odds)
  "Calculate next stake to return a profit on a losing run"
  (let ((new-stake 1))
	(my-while (<= (* new-stake odds)
				  (+ total-stake new-stake))
	  (incf new-stake))
	new-stake))

(defun get-new-stake (series-amount odds)
  (* series-amount
	 (/ 1 (1- odds))))

(defun calc-new-stake (series-amount odds)
  (if (>= odds 2)
	  series-amount
	  (ceiling (get-new-stake series-amount odds))))

(defun calc-stake (series-amount odds)
  (let* ((new-stake (calc-new-stake series-amount odds))
		 (new-return (* new-stake odds))
		 (old-return (* series-amount odds)))
	(format t "~%Series Amount : £ ~6,2f" series-amount)
	(format t "~%Return        : £ ~6,2f" old-return)
	(format t "~%Profit        : £ ~6,2f"(- old-return series-amount))
	(format t "~%~%New Stake     : £ ~6,2f" new-stake)
	(format t "~%Return        : £ ~6,2f" new-return)
	(format t "~%Profit        : £ ~6,2f"(- new-return new-stake))))

;; ***************************************************************
;; CSV file utilities
;;

(defun select-cols-from-row (wanted-list src-list)
  "select only columns in WANTED-LIST from each row of SRC-LIST,
   returns a list of the values from each wanted column"

  (let ((obj-list nil)
        (count 0))
    (labels ((inner (wanted src)
               (cond ((null wanted)
					  (reverse obj-list))
                     ((equal count (car wanted))
                      (push (car src) obj-list)
                      (incf count)
                      (inner (rest wanted) (rest src)))
                     (t (incf count)
                        (inner wanted (rest src))))))
      (inner wanted-list src-list))))

(defun find-header-columns (wanted-list src-list)
  "find the column headers in WANTED-LIST within SRC-LIST,
   returns numerical list of columns to be used by select-cols-from-row as wanted-list"

  (let ((new-list nil)
        (count 0))
    (labels ((inner (wanted src)
               (cond ((null wanted)
					  (reverse new-list))
                     ((string-equal (car wanted) (car src))
                      (push count new-list)
                      (incf count)
                      (inner (rest wanted) (rest src)))
                     (t (incf count)
;						(format t "~%{~a}" (rest wanted))
                        (inner wanted (rest src))))))
      (inner wanted-list src-list))))

(defun data-clean (cell &optional (value "2"))     ; value "1" causes division-by-zero errors in calc-new-stake ffs
  (let ((temp (ppcre:regex-replace "'" cell "")))  ; remove apostrophe in Nott'm Forest
    (ppcre:regex-replace "^$" temp value)))        ; fill in any blank cells

(defun transform-csv (file-from file-to columns)
  "Convert csv file FILE-FROM to new file FILE-TO only showing required columns from COLUMNS
   which can be either of the global variables *csv-cols* or *summer-csv-cols*"

  (let* ((data (import-csv file-from))
         (cols-list (find-header-columns columns (car data))))
    (export-csv
     (mapcar #'(lambda (row)
                 (mapcar #'(lambda (cell)
                             (format nil "~a" (data-clean cell)))
                         (select-cols-from-row cols-list row)))
             (cdr data)) ;; remove header line
     file-to)))

(defun update-files (leagues columns from-path to-path)
  "Transform updated CSV files to required format"
  (dolist (league leagues)
    (let ((from-str (format nil "~a/~a.csv" from-path (csv-filename league)))
          (to-str (format nil "~a/~a.csv" to-path (csv-filename league))))
      (format t "~%Writing ~a" to-str)
      (transform-csv from-str to-str columns))))

(defun update-csv-files ()
  (update-files *uk-leagues* *uk-csv-cols*
				"c:/mine/perl/football/data"
				"c:/mine/lisp/data"))


(defun update-euro-csv-files ()
  (update-files *euro-leagues* *uk-csv-cols*
				"c:/mine/perl/football/data/euro"
				"c:/mine/lisp/data"))

(defun update-summer-csv-files ()
  (update-files *summer-leagues* *summer-csv-cols*
				"c:/mine/perl/football/data/summer"
				"c:/mine/lisp/data"))

(defun update-all-csv-files ()
;  (update-csv-files)
;  (update-euro-csv-files)
  (update-summer-csv-files))

;; *******************************************
;; Date routines
;;

(defvar *months*
  '((01 31) (02 28) (03 31)
    (04 30) (05 31) (06 30)
    (07 31) (08 31) (09 30)
    (10 31) (11 30) (12 31)))

(defvar *days-to-add*
  '((0 3) ; Fri
    (1 2) ; Sat
    (2 1) ; Sun
    (3 0) ; Mon
    (4 2) ; Tue
    (5 1) ; Wed
    (6 0))) ; Thur

(defun get-days-in-month (m)
  "Get days in given calendar (M)onth"
  (second (assoc m *months*)))

(defun get-days-to-add (day)
  "Get number of days from DAY to end of weekend/midweek period"
  (second (assoc day *days-to-add*)))

(defun get-date (dt)
  (mod dt 100))

(defun get-month (dt)
  (truncate (/ (mod dt 10000) 100)))

(defun get-year (dt)
  (truncate (/ dt 10000)))

(defun simple-is-leap-year (y)
  "Only checks whether YEAR is divisible by 4"
  (zerop (mod y 4)))

(defun day-of-week (date month year)
  "Returns numerical day of week starting from Monday (0) to Sunday (6)"
  (nth-value 6 (decode-universal-time (encode-universal-time 0 0 0 date month year 0))))

(defun football-day-of-week (date month year)
  "Converts day-of-week to start from Friday (0) to Thursday (6)"
  (let ((d (+ 3 (day-of-week date month year))))
    (cond ((> d 6)
		   (- d 7))
          (t d))))

(defun get-next-date (dt)
  "Get start date of next football weekend/midweek period from current date DT"
  (let ((date (get-date dt))
        (month (get-month dt))
        (year (get-year dt)))
    (+= date (get-days-to-add (football-day-of-week date month year)))

    (when (> date (get-days-in-month month))
	  (if (and (equal month 2)
			   (simple-is-leap-year year))
		  (-= date 29)
		  (-= date (get-days-in-month month)))
	  (incf month)

	  (when (> month 12)
		(-= month 12)
		(incf year)))

	(+ (* year 10000)
       (* month 100)
       date)))

(defun transform-date (d)
  "Transform date from DD/MM/YYYY to ((YYYY)(MM)(DD)"
  (let ((x (ppcre:split "/" d)))
    (list (third x) (second x) (first x))))

(defun integer-date (date)
  "Transform date from DD/MM/YYYY to integer YYYYMMDD"
  (parse-integer (format nil "~{~a~}" (transform-date date))))

(defun todays-date ()
  (local-time:with-decoded-timestamp (:year y :month m :day d)
									 (local-time:now)
	(values y m d)))

(defun write-date-as-ymd (stream y m d)
  (format stream "~d-~2,'0d-~2,'0d" y m d))

;; **************************************************************
;; DSL Expects
;;

(defun build-stats (league-name league-hash)
  "Build stats for each LEAGUE-NAME, build in LEAGUE-HASH, called from do-stats in start-up"
  (dolist (team (get-teams league-name))
    (setf (gethash team league-hash) (make-stats)))
  (setf (gethash league-name *ht-league-stats*) (make-league-stats)))

(defun update-team-stats (hash game)
  "Used to update *ht-stats* structure during start-up"
  (let ((home-team (home-team game))
		(away-team (away-team game))
		(home-score (home-score game))
		(away-score (away-score game)))
	(set-home-for home-team hash home-score)
	(set-home-ag  home-team hash away-score)
	(set-away-for away-team hash away-score)
	(set-away-ag  away-team hash home-score)
	(incf (stats-home-games (gethash home-team hash)))
	(incf (stats-away-games (gethash away-team hash)))))

(defun update-league-stats (league-name game)
  "Used to update *ht-league-stats* structure during start-up"
  (set-league-stats-home-goals league-name (home-score game))
  (set-league-stats-away-goals league-name (away-score game))
  (incf (league-stats-games (gethash league-name *ht-league-stats*))))

(defun set-league-averages (league-name)
  "Used to calculate league averages for *ht-league-stats* once all games have been entered"
  (set-league-stats-av-home-goals league-name (/ (get-league-stats-home-goals league-name)
                                                 (get-league-stats-games league-name)))
  (set-league-stats-av-away-goals league-name (/ (get-league-stats-away-goals league-name)
                                                 (get-league-stats-games league-name))))

(defun do-stats (&optional (db *db*))
  "Calculates stats for all teams"
  (dolist (league db)
    (let ((league-hash (make-hash-table :test #'equal))
          (league-name (csv-filename league)))
      (build-stats league-name league-hash)

      (dolist (game (cdr league))
        (update-team-stats league-hash game)
        (update-league-stats league-name game))

      (setf (gethash league-name *ht-stats*) league-hash)
      (set-league-averages league-name))))

(defun do-skellam (home-expect away-expect)
  "Calculate skellam distribution odds for game HOME-EXPECT and AWAY-EXPECT - not used"
  (let ((sk (make-instance 'my-skellam :margin 6)))
    (calc-skellam sk home-expect away-expect)
    (sk-show-hash sk)
	(multiple-value-bind (home draw away)
		(vget-odds sk)
	  (format t "~%Home : ~$~%Draw : ~$~%Away : ~$" home draw away))))

(defun calc-expects ()
  "Calculates goal expectancy values for all teams"
  (iter (for (league-name league-hash) in-hashtable *ht-stats*)
    (let ((av-home-goals (get-league-stats-av-home-goals league-name))
          (av-away-goals (get-league-stats-av-away-goals league-name)))

      (iter (for (team stats) in-hashtable league-hash) ;; stats not used below
        (let* ((_home-games (get-home-games team league-hash)) ; to avoid accessing league-hash twce
			   (_away-games (get-away-games team league-hash))
			   (home-games (if (> _home-games 0) _home-games 1)) 
			   (away-games (if (> _away-games 0) _away-games 1)))
		  (set-av-home-for team league-hash (/ (get-home-for team league-name) home-games))
		  (set-av-home-ag  team league-hash (/ (get-home-ag  team league-name) home-games))
		  (set-av-away-for team league-hash (/ (get-away-for team league-name) away-games))
		  (set-av-away-ag  team league-hash (/ (get-away-ag  team league-name) away-games))

		  (set-expect-home-for team league-hash (/ (get-av-home-for team league-name) av-home-goals))
		  (set-expect-home-ag  team league-hash (/ (get-av-home-ag  team league-name) av-away-goals))
		  (set-expect-away-for team league-hash (/ (get-av-away-for team league-name) av-away-goals))
		  (set-expect-away-ag  team league-hash (/ (get-av-away-ag  team league-name) av-home-goals)))))))

(defun do-game-expect (league home-team away-team)
  "Calculates goal expectancy values for individual fixture"
  (let ((temp (make-game)))
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
    temp))

(defun do-expects ()
  "Calculates goal expectancy values for each fixture"
  (load-fixtures)
  (when (null *fixtures*)
	(return-from do-expects nil))
  (setf *expects* nil)
  (dolist (game *fixtures*)
    (push (do-game-expect (fleague game) (fhome game) (faway game)) *expects*)))

(defun game-odds (league home-team away-team &key (size 10))
  (let ((game (do-game-expect league home-team away-team))
		(ds (make-instance 'my-odds :size size)))
	(format t "~%~a v ~a ~%Home : ~,2f ~%Away : ~,2f ~%" home-team away-team (game-home-goals game) (game-away-goals game))
	(my-describe2 ds (game-home-goals game) (game-away-goals game))))

(defun game-ou-odds (league home-team away-team &key (size 10))
  (let ((game (do-game-expect league home-team away-team))
		(ds (make-instance 'my-odds :size size)))
	(format t "~%~a v ~a ~%Home : ~,2f ~%Away : ~,2f ~%" home-team away-team (game-home-goals game) (game-away-goals game))
	(my-describe-ou ds (game-home-goals game) (game-away-goals game))))

(defun sort-expects ()
  (do-expects)
  (setf *sorted* (safe-sort *expects*
                   #'> :key #'game-goal-diff)))

(defun get-league-odds (league)
  (let ((my-list nil))
	(mapcar #'(lambda (game)
				(when (string-equal (game-league game) league)
				  (push game my-list)))
			*expects*)
	my-list))

(defun league-odds (league &key (gtlt-fn #'>)
								(cmp-fn #'game-over-odds))
  (print-game-odds (sort (get-league-odds league) gtlt-fn :key cmp-fn)))

(defun calc-game-odds (&key (games *expects*) (size 6))
  "Calculates odds for each game from goal expectancy values"
  (dolist (game games)
    (let ((ds (make-instance 'my-odds :size size)))
      (calc-game ds (game-home-goals game) (game-away-goals game))
	  (setf (values (game-home-odds game)
					(game-draw-odds game)
					(game-away-odds game)
					(game-over-odds game)
					(game-under-odds game))
			(vget-odds ds))))
  games)

(defun do-odds (&key (games *expects*) (cmp-fn #'game-over-odds) (size 6))
  (format t "~%")
  (print-game-odds (safe-sort (calc-game-odds :games games :size size) #'> :key cmp-fn)))

(defun diff-ou-odds-fn (game)
  (abs (- (game-over-odds game)
		  (game-under-odds game))))

(defun diff-result-odds-fn (game)
  (abs (- (game-home-odds game)
		  (game-away-odds game))))

(defun do-diff-odds (games n diff-odds-fn cmp-fn)
  (print-game-odds (first-n n
							(safe-sort (calc-game-odds :games games) cmp-fn :key diff-odds-fn))))

(defun diff-ou-odds (&key (games *expects*) (n 20) (cmp-fn #'>))
  (do-diff-odds games n #'diff-ou-odds-fn cmp-fn))

(defun diff-result-odds (&key (games *expects*) (n 20) (cmp-fn #'>))
  (do-diff-odds games n #'diff-result-odds-fn cmp-fn))

(defun get-date-as-string (game)
  (subseq (fdate-time game) 3 8))

(defun get-time-as-string (game)
  (subseq (fdate-time game) 9 14))

(defun get-time (game)
  (let ((game-time (get-time-as-string game)))
	(+ (* 100 (parse-integer (subseq game-time 0 2)))
	   (parse-integer (subseq game-time 3 5)))))

(defun date-odds-fn (date &optional (size 6))
  (let ((my-expects nil))
	(mapcar #'(lambda (game)
				(when (string-equal (subseq (fdate-time game) 3 8)
									(subseq date 0 5)) ;; to allow date to be entered as either DD/MM or DD/MM/YY
				  (push (do-game-expect (fleague game) (fhome game) (faway game)) my-expects)))
			*fixtures*)
	(calc-game-odds :games my-expects :size size)))

(defun early-odds-fn (date &optional (size 6))
  (let ((my-expects nil))
	(mapcar #'(lambda (game)
				(when (and (string-equal (get-date-as-string game) date)
						   (< (get-time game) 1500))
				  (push (do-game-expect (fleague game) (fhome game) (faway game)) my-expects)))
			*fixtures*)
	(calc-game-odds :games my-expects :size size)))

(defun day-odds-fn (date &optional (size 6))
  (let ((my-expects nil))
	(mapcar #'(lambda (game)
				(when (and (string-equal (get-date-as-string game) date)
						   (> (get-time game) 1429)
						   (< (get-time game) 1631))
				  (push (do-game-expect (fleague game) (fhome game) (faway game)) my-expects)))
			*fixtures*)
	(calc-game-odds :games my-expects :size size)))

(defun late-odds-fn (date &optional (size 6))
  (let ((my-expects nil))
	(mapcar #'(lambda (game)
				(when (and (string-equal (get-date-as-string game) date)
						   (> (get-time game) 1650))
				  (push (do-game-expect (fleague game) (fhome game) (faway game)) my-expects)))
			*fixtures*)
	(calc-game-odds :games my-expects :size size)))

(defmacro do-odds-fn (fn-name odds-fn)
  `(defun ,fn-name (date &key (cmp-fn #'game-over-odds) (size 6))
	 (print-game-odds
	  (sort (funcall ,odds-fn date size) #'> :key cmp-fn))))

(do-odds-fn date-odds #'date-odds-fn)
(do-odds-fn early-odds #'early-odds-fn)
(do-odds-fn day-odds #'day-odds-fn)
(do-odds-fn late-odds #'late-odds-fn)

(defun expect-goals (game)
  (+ (game-home-goals game)
	 (game-away-goals game)))

(defun under-expects (&optional (goals 1))
  (let ((my-list nil))
	(dolist (game *expects*)
	  (when (and (< (game-home-goals game) goals)
				 (< (game-away-goals game) goals))
		(push game my-list)))
	
	(dolist (game (safe-sort my-list #'< :key #'expect-goals))
	  (format t "~% ~3a : ~a v ~a ~37tHome : ~,2f Away : ~,2f - ~,2f"
			  (string-upcase (game-league game))
			  (game-home-team game) (game-away-team game)
			  (game-home-goals game) (game-away-goals game)
			  (+ (game-home-goals game) (game-away-goals game)))))
  t)

(defun over-expects (&optional (goals 3))
  (let ((my-list nil))
	(dolist (game *expects*)
	  (when (> (expect-goals game)
			   goals)
		(push game my-list)))
	
	(dolist (game (safe-sort my-list #'> :key #'expect-goals))
	  (format t "~% ~3a : ~a v ~a ~37tHome : ~,2f Away : ~,2f - ~,2f"
			  (string-upcase (game-league game))
			  (game-home-team game) (game-away-team game)
			  (game-home-goals game) (game-away-goals game)
			  (expect-goals game))))
  t)

(defun nil-expects ()
  (let ((ds (make-instance 'my-odds :size 10))
		(my-list nil))
	(dolist (game *expects*)
	  (push (list (string-upcase (game-league game))
				  (game-home-team game)
				  (game-away-team game)
				  (parse-float (calc-nil-expects ds (game-home-goals game) (game-away-goals game))))
			my-list))

	(dolist (game (sort my-list #'< :key #'fourth))
	  (format t "~{~% ~3a : ~a v ~a ~36t: ~6,2f ~}" game)))
  t)

(defun write-to-org-file (my-list)
  (with-open-file (stream "C:/Users/Steve/Dropbox/org/overs.org"
						  :direction :output
						  :if-exists :append ;:if-exists :supersede
						  :if-does-not-exist :create)
	
	(format stream "~%* ")
	(multiple-value-bind (y m d) (todays-date)
		 (write-date-as-ymd stream y m d))
	(format stream "~%** OVER 3.5")

	(dolist (game my-list)
	  (format stream "~%~a v ~a ~,2f : ~,2f"
			  (game-home-team game) (game-away-team game)
			  (game-home-goals game) (game-away-goals game)))))

(defun over3.5-odds (&key (games *expects*)
						  (cmp-fn #'game-over-odds)
						  (write-org-file nil))
  "Print all games where over 3.5 odds are under 2.0"
  (let ((my-list nil)
		(sorted-list nil))

	(dolist (game games)
	  (let ((ds (make-instance 'my-odds :size 10)))
		(calc-game ds (game-home-goals game) (game-away-goals game))
		(when (< (over3.5 ds) 2)
		  (push game my-list))))

	(setf sorted-list (sort my-list
							#'> :key cmp-fn))
	(print-game-odds  sorted-list)

	(when (equal write-org-file t)
	  (write-to-org-file sorted-list))))

(defun start ()
  "Load all data"
  (clear-database)
  (import-teams)
  (load-leagues)
  (load-fixtures)
  (load-my-teams)
  (do-stats))

(defun start+ (&key (odds t))
  "Load all data, calculate goal expects and odds"
  (start)
  (calc-expects)
  (sort-expects)
  (when (equal odds t)
	(do-odds))
  t)

(defun switch-uk (&key (odds t))
  (defparameter *leagues* *uk-leagues*)
  (defparameter *fixtures-file* *uk-fixtures-file*)
  (defparameter *csv-cols* *uk-csv-cols*)
  (defparameter *teams-file* *uk-teams-file*)
  (start+ :odds odds))

(defun switch-euro (&key (odds t))
  (defparameter *leagues* *euro-leagues*)
  (defparameter *fixtures-file* *euro-fixtures-file*)
  (defparameter *csv-cols* *uk-csv-cols*)
  (defparameter *teams-file* *euro-teams-file*)
  (start+ :odds odds))

(defun switch-summer (&key (odds t))
  (defparameter *leagues* *summer-leagues*)
  (defparameter *fixtures-file* *summer-fixtures-file*)
  (defparameter *csv-cols* *summer-csv-cols*)
  (defparameter *teams-file* *summer-teams-file*)
  (start+ :odds odds))

(defun uk ()
  (switch-uk :odds nil))

(defun euro ()
  (switch-euro :odds nil))

(defun summer ()
  (switch-summer :odds nil))

(defun switch-rugby ()
  (defparameter *leagues* *rugby-leagues*)
  (defparameter *fixtures-file* *rugby-fixtures-file*)
  (defparameter *teams-file* *rugby-teams-file*)
  (defparameter *csv-cols* *rugby-csv-cols*)
  (import-teams)
  (import-rugby-csv)
  (start+))

;; *******************************************
;; Season stats
;;

(defun do-season (csv-league)
  "Iterate through each CSV-LEAGUE season, breaking data into seperate lists
  for each weekend/midweek period"
  (let* ((league (get-league csv-league))
         (start-date (integer-date (caar league)))
         (week-list nil)
         (season-list nil))

    (labels ((inner (my-list)
               (cond ((null my-list)
                      (push (reverse week-list) season-list)
                      (reverse season-list))
                     ((<= (integer-date (date (car my-list))) (get-next-date start-date)) ; fits within this week
                      (push (car my-list) week-list)
                      (inner (rest my-list)))
                     (t  ; this must be a new week
                      (push (reverse week-list) season-list)
                      (cond ((null (cdr my-list)) ; only one game for this week at the very end of the list
                             (push (car my-list) week-list)
                             (inner (rest my-list))) ; go to null list above and exit
                            (t ; start a new week
                             (setf start-date (integer-date (date (car my-list))))
                             (setf week-list nil)
                             (inner my-list))))))) ; haven't done anything with this line yet
      (inner league))))

(defun do-season-by-week (league-name)
  "Print each weekend/midweek period for CSV-LEAGUE"
  (dolist (week (do-season league-name))
    (print week)
    (read-line)))

(defun get-expect-key (x)
  "Convert goal-expect goal-difference value to string representation with only one fractional digit"
  (format nil "~,1f" x))

(defun add-to-expects-db (expect result)
  "Add EXPECT value and RESULT to *expects-db* hash-table"
  (let ((expect (get-expect-key (my-floor (game-goal-diff expect) 0.5))))

	(or (gethash expect *expects-db*)
		(setf (gethash expect *expects-db*)
			  (make-instance 'my-expectdb :expect expect)))
	(incr-stats (gethash expect *expects-db*) result)))

(defun do-season-expects (league-name)
  "Process all games for CSV-LEAGUE, calculate expect values and add to *expect-db* hash table"

  (let ((league-hash (make-hash-table :test #'equal))
        (week-count 0))

    (build-stats league-name league-hash)
    (dolist (week (do-season league-name))
      (incf week-count)
      (dolist (game week)
        (when (> week-count 6)
		  (calc-expects)
		  (let ((expect (do-game-expect league-name (home-team game) (away-team game))))
			(add-to-expects-db expect (result game))))

        (update-team-stats league-hash game)
        (update-league-stats league-name game))
      (setf (gethash league-name *ht-stats*) league-hash)
      (set-league-averages league-name))))

(defun show-ordered-expects ()
  "Show sorted contents of *expects-db* hash table"
  (format t "~%")
  (my-for i (-10 10 :step 0.5)
	(let ((key (get-expect-key i)))
	  (if (gethash key *expects-db*)
		  (my-describe (gethash key *expects-db*))))))

(defun do-season-expects-by-league (league)
  "Calculate all expects for LEAGUE and show sorted database"
  (start)
  (do-season-expects league)
  (show-ordered-expects))

(defun do-season-expects-all ()
  "Call expects for all leagues and show sorted database"
  (start+)
  (dolist (league *leagues*)
    (do-season-expects (csv-filename league)))
  (show-ordered-expects))


;; *******************************************
;; Series stats
;;

(defmacro with-all-teams ((team leagues) &body body)
  `(dolist (league ,leagues) ;;exposes league for capture by macro user
	 (dolist (,team (get-teams (csv-filename league)))
	   ,@body)))

(defun consecutive-games (fn test-func)
  "Used by unbeaten and consecutive-draw functions below, shows all teams on an unbeaten home and/or away run.
   unbeaten-home-aways and since-last-win are the only functions that use 'team' passed into the 'inner' function,
   all other functions have to work around this"

  (labels ((inner (team games count)
			 (cond ((null games) count)
				   ((funcall test-func team (car games))
					(inner team (rest games) (1+ count)))
				   (t count))))

	(let ((my-list nil))
	  (with-all-teams (team *leagues*)
		(let* ((games (reverse (funcall fn team)))
			   (wins (inner team games 0)))
		  (when (>= wins 2)
			(push (list (string-upcase (csv-filename league))
						team
						wins)
				  my-list))))
	  (sort my-list #'> :key #'third))))

(defun unbeaten-table (my-list)
  (format-table t my-list
				:column-label '("League" "Team" "Games")
				:column-align '(:center :center :center)))

(defmacro cons-games1 (fn-name games-fn test-fn)
  `(defun ,fn-name (&optional (n 10))
	 (unbeaten-table (first-n n (consecutive-games ,games-fn
												  #'(lambda (team game)
													  ,test-fn))))))

(cons-games1 unbeaten-home-aways
			 #'home-aways (home-away-win-result team game))
(cons-games1 since-last-win
			 #'home-aways (not-home-away-win-result team game))


(defmacro cons-games2 (fn-name games-fn test-fn)
  `(defun ,fn-name (&optional (n 10))
	 (unbeaten-table (first-n n (consecutive-games ,games-fn
												   #'(lambda (team game)
													   (declare (ignore team))
													   ,test-fn))))))

(cons-games2 since-last-home-win
			 #'homes (string-ne (result game) "H"))
(cons-games2 since-last-away-win
			 #'aways (string-ne (result game) "A"))
(cons-games2 since-last-draw
			 #'home-aways (string-ne (result game) "D"))

(cons-games2 since-last-over
			 #'home-aways (is-under game))
(cons-games2 since-last-home-over
			 #'homes (is-under game))
(cons-games2 since-last-away-over
			 #'aways (is-under game))

(cons-games2 since-last-under
			 #'home-aways (is-over game))
(cons-games2 since-last-home-under
			 #'homes (is-over game))
(cons-games2 since-last-away-under
			 #'aways (is-over game))

(defun unbeaten (fn result)
  (unbeaten-table
   (consecutive-games fn #'(lambda (team game)
							 (declare (ignore team)) ; work-around
							 (equal result (result game))))))

(defun unbeaten-homes ()
  (unbeaten #'homes "H"))
(defun unbeaten-aways ()
  (unbeaten #'aways "A"))

(defun consecutive-home-draws ()
  (unbeaten #'homes "D"))
(defun consecutive-away-draws ()
  (unbeaten #'aways "D"))
(defun consecutive-draws ()
  (unbeaten #'home-aways "D"))

(defun show-unbeaten ()
  (format t "~%HOMES :~%")
  (unbeaten-homes)
  (format t "~%AWAYS :~%")
  (unbeaten-aways)
  (format t "~%ALL :~%")
  (unbeaten-home-aways))

(defun show-consecutive-draws ()
  (format t "~%HOMES :~%")
  (consecutive-home-draws)
  (format t "~%AWAYS :~%")
  (consecutive-away-draws)
  (format t "~%ALL :~%")
  (consecutive-draws)
  (format t "~%~%SINCE LAST DRAW :~%")
  (since-last-draw))

(defun count-season (games-fn test-fn)
  "Returns a list of all teams sorted by the games returned by FN
   which match the result TEST-FN"

  (labels ((inner (games count)
			 (cond ((null games) count)
				   ((funcall test-fn (car games))
					(inner (rest games) (1+ count)))
				   (t (inner (rest games) count)))))

	(let ((my-list nil))
	  (with-all-teams (team *leagues*)
		(let* ((games (funcall games-fn team))
			   (count (inner games 0)))
 		  (push (list team
					  count
 					  (length games)
					  (calc-percent (length games) count))
				my-list)))

	  (sort my-list #'> :key #'fourth))))

(defun count-games-table (my-list)
  (format-table t my-list
				:column-label '("Team" "Wins" "Games" "Percents")
				:column-align '(:left :center :center :center)))

(defun count-games (games-fn result n)
  (count-games-table
   (first-n n (count-season
			   games-fn
			   #'(lambda (game)
				   (equal result (result game)))))))

(defun count-home-draws (&optional (n 10))
  (count-games #'homes "D" n))
(defun count-home-wins (&optional (n 10))
  (count-games #'homes "H" n))
(defun count-home-defeats (&optional (n 10))
  (count-games #'homes "A" n))

(defun count-away-draws (&optional (n 10))
  (count-games #'aways "D" n))
(defun count-away-wins (&optional (n 10))
  (count-games #'aways "A" n))
(defun count-away-defeats (&optional (n 10))
  (count-games #'aways "H" n))

(defun count-draws (&optional (n 10))
  (count-games #'home-aways "D" n))

(defun count-ou (games-fn test-fn n goals)
  (count-games-table
   (first-n n (count-season
			   games-fn
			   #'(lambda (game)
				   (funcall test-fn game goals))))))

(defun count-overs (&key (n 10) (goals 2.5))
  (count-ou #'home-aways #'is-over n goals))
(defun count-home-overs (&key (n 10) (goals 2.5))
  (count-ou #'homes #'is-over n goals))
(defun count-away-overs (&key (n 10) (goals 2.5))
  (count-ou #'aways #'is-over n goals))

(defun count-unders (&key (n 10) (goals 2.5))
  (count-ou #'home-aways #'is-under n goals))
(defun count-home-unders (&key (n 10) (goals 2.5))
  (count-ou #'homes #'is-under n goals))
(defun count-away-unders (&key (n 10) (goals 2.5))
  (count-ou #'aways #'is-under n goals))

;; *******************************************
;; DSL Series
;;

(defun make-circular-list (my-list)
  (setf (cdr (last my-list)) my-list))

(defun make-old-circular-series (start-list circular-list)
  (setf *print-circle* t)
  (let* ((y (make-circular-list circular-list))
		 (x (append (copy-list start-list) y)))			 
	
	#'(lambda (&optional (result ""))
		(cond ((or (string-equal result "W")
				   (string-equal result "R"))
			   (setf x (append (copy-list start-list) y)))
			  ((string-equal result "L")

			   (setf x (rest x)))
			  ((string-equal result "P")
			   (print start-list)
			   (print y))			  
			  (t (car x))))))

(defun make-circular-series (start-list circular-list)
  (setf *print-circle* t)
  (let* ((y (make-circular-list circular-list))
		 (x (append (copy-list start-list) y))
		 (count 0))			 
	
	#'(lambda (&optional (result ""))
		(if (string-equal result "W")
			(incf count))
		
		(cond ((or (= count 2)
				   (string-equal result "R"))
			   (setf count 0)
			   (setf x (append (copy-list start-list) y))
			   (car x))			  
			  ((string-equal result "")
			   (car x))
			  ((string-equal result "P")
			   (print start-list)
			   (print y))			  
			  (t (setf x (rest x))
				 (car x))))))

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

(defun make-long-series (my-list)
  (let ((x (copy-list my-list)))

	#'(lambda (&optional (result ""))
		(cond ((or (string-equal result "W")
				   (string-equal result "R"))
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

(defun make-23-series (my-list)
  (let ((idx 0)
		(wins 0)
		(games 0))
	
	#'(lambda (&optional (result ""))
		(labels ((reset-series ()
				   (setf idx 0)
				   (setf wins 0)
				   (setf games 0)))

		  (cond ((string-equal result "R")
				 (reset-series))

				((string-equal result "")
				 (nth idx my-list))

				((string-equal result "W")
				 (if (= idx 0)
					 (incf idx)
					 (decf idx))
				 (incf wins)
				 (incf games)
				 (if (= wins 2)
					 (reset-series)))
				
				(t ;; (string-equal result "L"
				 (incf idx)
				 (when (= games 2)
				   (setf games 0)
				   (decf wins))
				 (when (= games 1)
				   (incf games))))

		  (when (null (nth idx my-list))
			(reset-series))
		  (nth idx my-list)))))

(defun series-test (series)
  (let ((in ""))
	(print (funcall series "R"))
	(my-while (string-ne "X" (input in ">"))
	  (print (funcall series in)))))

(defparameter s1 (make-circular-series '(1 1) '(1 1)))
(defparameter s2 (make-circular-series '(2 2) '(2 2)))
(defparameter s3 (make-circular-series '(3 3) '(3 3)))

(defparameter s246 (make-23-series '(2 4 6 8 10 12)))
(defparameter s248 (make-23-series '(2 4 8 12 16 24)))
(defparameter s369 (make-23-series '(3 6 9 12 15 18)))
(defparameter stoffo (make-short-series '(11 22 44 66 88 132)))

(defun series-current (series)
  (funcall series))
(defun series-update (series result)
  (funcall series result))
(defun series-reset (series)
  (funcall series "R"))
(defun series-print (series)
  (funcall series "P"))

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
		  (let* ((current (series-current s))
				 (odds (funcall odds-fn team game))
				 (my-stake (calc-new-stake current odds)))
			(+= stake my-stake)
			(cond ((funcall result-fn team game)
				   (+= returns (* my-stake odds))
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

(defun series-table (n my-list)
  (format-table t (first-n n my-list)
				:column-label '("League" "Team" "Stake" "Return" "Percents")
				:column-align '(:center :left :center :center :center)))

(defun do-series-wins-calc (series)
  (do-series series #'home-aways #'home-away-win-result #'home-away-odds))
(defun do-series-home-wins-calc (series)
  (do-series series #'homes #'home-away-win-result #'home-away-odds))
(defun do-series-away-wins-calc (series)
  (do-series series #'aways #'home-away-win-result #'home-away-odds))

(defun do-series-defeats-calc (series)
  (do-series series #'home-aways #'home-away-defeat-result #'home-away-lost-odds))
(defun do-series-home-defeats-calc (series)
  (do-series series #'homes #'home-away-defeat-result #'home-away-lost-odds))
(defun do-series-away-defeats-calc (series)
  (do-series series #'aways #'home-away-defeat-result #'home-away-lost-odds))

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

(defun do-series-last-six-overs-calc (series)
  (do-series series #'last-six #'series-overs #'series-over-odds))
(defun do-series-last-six-unders-calc (series)
  (do-series series #'last-six #'series-unders #'series-under-odds))

(defun do-series-wins (series &optional (n 10))
  (series-table n (do-series-wins-calc series)))
(defun do-series-home-wins (series &optional (n 10))
  (series-table n (do-series-home-wins-calc series)))
(defun do-series-away-wins (series &optional (n 10))
  (series-table n (do-series-away-wins-calc series)))

(defun do-series-defeats (series &optional (n 10))
  (series-table n (do-series-defeats-calc series)))
(defun do-series-home-defeats (series &optional (n 10))
  (series-table n (do-series-home-defeats-calc series)))
(defun do-series-away-defeats (series &optional (n 10))
  (series-table n (do-series-away-defeats-calc series)))

(defun do-series-draws (series &optional (n 10))
  (series-table n (do-series-draws-calc series)))
(defun do-series-home-draws (series &optional (n 10))
  (series-table n (do-series-home-draws-calc series)))
(defun do-series-away-draws (series &optional (n 10))
  (series-table n (do-series-away-draws-calc series)))

(defun do-series-overs (series &optional (n 10))
  (series-table n (do-series-overs-calc series)))
(defun do-series-home-overs (series &optional (n 10))
  (series-table n (do-series-home-overs-calc series)))
(defun do-series-away-overs (series &optional (n 10))
  (series-table n (do-series-away-overs-calc series)))

(defun do-series-unders (series &optional (n 10))
  (series-table n (do-series-unders-calc series)))
(defun do-series-home-unders (series &optional (n 10))
  (series-table n (do-series-home-unders-calc series)))
(defun do-series-away-unders (series &optional (n 10))
  (series-table n (do-series-away-unders-calc series)))

(defun do-series-last-six-overs (series &optional (n 10))
  (series-table n (do-series-last-six-overs-calc series)))
(defun do-series-last-six-unders (series &optional (n 10))
  (series-table n (do-series-last-six-unders-calc series)))


(defparameter series-list `((,s1 "s1")
							(,s246 "s246")
							(,s369 "s369")
							(,stoffo "stoffo")))

(defun do-home-away-series (s games-fn home-result-fn away-result-fn
							&optional (home-odds-fn #'summer-ou-odds)
									  (away-odds-fn #'summer-ou-odds))
  "Returns a list of all teams sorted by their returns using series S given the 
   games returned by GAMES-FN which match results from HOME-RESULT-FN at odds HOME-ODDS-FN
   and AWAY-RESULT-FN at odds AWAY-ODDS-FN to enable backing either home-overs and away-unders
   or away-overs and home-unders"
  
  (let ((my-list nil))
	(with-all-teams (team *leagues*)
	  (let ((stake 0)
			(returns 0)
			(result ""))
		(series-reset s)
		(dolist (game (funcall games-fn team))
		  (let ((current (series-current s)))
			
			(cond ((is-home team game)
				   (let* ((odds (funcall home-odds-fn team game))
						  (my-stake (calc-new-stake current odds)))
					 (+= stake my-stake)
					 (cond ((funcall home-result-fn team game)
							(+= returns (* my-stake odds))
							(setf result "W"))
						   (t (setf result "L")))))
				  (t (let* ((odds (funcall away-odds-fn team game))
							(my-stake (calc-new-stake current odds)))
					   (+= stake my-stake)
					   (cond ((funcall away-result-fn team game)
							  (+= returns (* my-stake odds))
							  (setf result "W"))
							 (t (setf result "L"))))))
			(series-update s result)))
		
		(when (> stake 0)
		  (push (list (string-upcase (csv-filename league))
					  team
					  stake
					  (format nil "~6,2f" returns)
					  (calc-percent stake returns))
				my-list))))

	(sort my-list #'> :key #'fifth)))

(defun do-series-over-unders-calc (series)
  (do-home-away-series series #'home-aways #'series-overs #'series-unders #'series-over-odds #'series-under-odds))
(defun do-series-under-overs-calc (series)
  (do-home-away-series series #'home-aways #'series-unders #'series-overs #'series-under-odds #'series-over-odds))

(defun do-summer-series-over-unders-calc (series)
  (do-home-away-series series #'home-aways #'series-overs #'series-unders))
(defun do-summer-series-under-overs-calc (series)
  (do-home-away-series series #'home-aways #'series-unders #'series-overs))

(defun do-series-over-unders (series &optional (n 10))
  (series-table n (do-series-over-unders-calc series)))
(defun do-series-under-overs (series &optional (n 10))
  (series-table n (do-series-under-overs-calc series)))

(defun do-summer-series-over-unders (series &optional (n 10))
  (series-table n (do-summer-series-over-unders-calc series)))
(defun do-summer-series-under-overs (series &optional (n 10))
  (series-table n (do-summer-series-under-overs-calc series)))

(defun last-six-home-aways (team)
  (append (last-six-homes team) (last-six-aways team)))
 
(defun do-last-six-over-unders-calc (&optional (series s1))
  (do-home-away-series series #'last-six-home-aways #'series-overs #'series-unders #'series-over-odds #'series-under-odds))
(defun do-last-six-under-overs-calc (&optional (series s1))
  (do-home-away-series series #'last-six-home-aways #'series-unders #'series-overs #'series-under-odds #'series-over-odds))

(defun do-summer-last-six-over-unders-calc (&optional (series s1))
  (do-home-away-series series #'last-six-home-aways #'series-overs #'series-unders))
(defun do-summer-last-six-under-overs-calc (&optional (series s1))
  (do-home-away-series series #'last-six-home-aways #'series-unders #'series-overs))

(defun do-last-six-over-under-returns (&optional (n 10))
  (series-table n (do-last-six-over-unders-calc)))
(defun do-last-six-under-over-returns (&optional (n 10))
  (series-table n (do-last-six-under-overs-calc)))

(defun do-summer-last-six-over-under-returns (&optional (n 10))
  (series-table n (do-summer-last-six-over-unders-calc)))
(defun do-summer-last-six-under-over-returns (&optional (n 10))
  (series-table n (do-summer-last-six-under-overs-calc)))

(defun do-series-win-loss-calc (series)
  (do-home-away-series series #'home-aways #'is-win #'is-defeat #'series-home-odds #'series-home-odds))
(defun do-series-loss-win-calc (series)
  (do-home-away-series series #'home-aways #'is-defeat #'is-win #'series-away-odds #'series-away-odds))

(defun do-series-win-loss (series &optional (n 10))
  (series-table n (do-series-win-loss-calc series)))
(defun do-series-loss-win (series &optional (n 10))
  (series-table n (do-series-loss-win-calc series)))

;; *******************************************
;; DSL Best Series
;;

(defparameter series-results '("Wins" "Defeats" "Draws" "Win Loss" "Loss Win"))
(defparameter series-fns (list #'home-away-win-result
							   #'home-away-defeat-result
							   #'home-away-draw-result
							   #'win-loss-result
							   #'loss-win-result))
(defparameter series-odds-fns (list #'home-away-odds
									#'home-away-lost-odds
									#'series-draw-odds
									#'series-win-loss-odds
									#'series-loss-win-odds))

(defparameter series-ou-results '("Overs" "Unders" "Over Unders" "Under Overs"))
(defparameter series-ou-fns (list #'series-overs
								  #'series-unders
								  #'series-over-unders
								  #'series-under-overs))
(defparameter series-ou-odds-fns (list #'series-over-odds
									   #'series-under-odds
									   #'series-ou-odds
									   #'series-uo-odds))


(defun series-all-table (my-list)
  (format-table t my-list
				:column-label '("Result" "League" "Team" "Stake" "Return" "Percents")
				:column-align '(:center :left :center :center :center :center))
  t)

(defun do-series-all-results (series games-fn n)
  (let ((my-list nil))
	(mapcar #'(lambda (result result-fn odds-fn)
				(dolist (results (do-series series games-fn result-fn odds-fn))
				  (push (cons result results) my-list)))
			series-results series-fns series-odds-fns)
	
	(series-all-table
	 (first-n n (sort my-list #'> :key #'sixth)))))

(defun do-series-all (series &optional (n 10))
  (do-series-all-results series #'home-aways n))
(defun do-series-all-homes (series &optional (n 10))
  (do-series-all-results series #'homes n))
(defun do-series-all-aways (series &optional (n 10))
  (do-series-all-results series #'aways n))

(defun do-series-all-last-six (series &optional (n 10))
  (do-series-all-results series #'last-six n))
(defun do-series-all-last-six-homes (series &optional (n 10))
  (do-series-all-results series #'last-six-homes n))
(defun do-series-all-last-six-aways (series &optional (n 10))
  (do-series-all-results series #'last-six-aways n))

(defun calc-team-series (team series games-fn result-fn odds-fn)
  "Adapted from do-team-series, calculates returns of a TEAM with series S,
   using games returned from GAMES-FN which match RESULT-FN at odds ODDS-FN"
  
  (let ((stake 0)
		(returns 0)
		(result ""))
	(series-reset series)
	
	(dolist (game (funcall games-fn team))
	  (let* ((current (series-current series))
			 (odds (funcall odds-fn team game))
			 (my-stake (calc-new-stake current odds)))
		(+= stake my-stake)
		(cond ((funcall result-fn team game)
			   (+= returns (* my-stake odds))
			   (setf result "W"))
			  (t (setf result "L")))
		(series-update series result)))
	(values stake returns)))

(defun best-series-table (my-list)
  (format t "~%")
  (format-table t my-list
				:column-label '("Result" "Stake" "Return" "Percents")
				:column-align '(:center :center :center :center))
  t)

(defun best-series (team series &optional (n 15))
  "Calculates best results for given TEAM for all series and results"
  (let ((my-list nil))
	(mapcar #'(lambda (result result-fn odds-fn)
				(multiple-value-bind (stake returns)
					(calc-team-series team series #'home-aways result-fn odds-fn)
				  (push (list result
							  stake
							  (my-round returns 0.01)
							  (calc-percent stake returns))
						my-list)))
			series-results series-fns series-odds-fns)
	
	(best-series-table
	 (first-n n (sort my-list #'> :key #'fourth)))))

(defun best-ou-series (team series &optional (n 15))
  "Calculates best results for given TEAM for all series and results"
  (let ((my-list nil))
	(mapcar #'(lambda (result result-fn odds-fn)
				(multiple-value-bind (stake returns)
					(calc-team-series team series #'home-aways result-fn odds-fn)
				  (push (list result
							  stake
							  (my-round returns 0.01)
							  (calc-percent stake returns))
						my-list)))
			series-ou-results series-ou-fns series-ou-odds-fns)
	
	(best-series-table
	 (first-n n (sort my-list #'> :key #'fourth)))))

(defun do-all-series-table (my-list)
  (format t "~%")
  (format-table t my-list
				:column-label '("League" "Team" "Result" "Stake" "Return" "Percents")
				:column-align '(:center :center :center :center :center :center))
  t)

(defun do-all-series-calc (series n results results-fns odds-fns &optional (games #'home-aways))
  "Produce a list of the best returns from all teams for results given by RESULTS-FNS"

  (let ((my-list nil))
	(with-all-teams (team *leagues*)
	  (mapcar #'(lambda (result result-fn odds-fn)
				  (multiple-value-bind (stake returns)
					  (calc-team-series team series games result-fn odds-fn)
					(push (list (string-upcase (csv-filename league))
								team
								result
								stake
								(my-round returns 0.01)
								(calc-percent stake returns))
						  my-list)))
			  results results-fns odds-fns))

	(first-n n (sort my-list #'> :key #'sixth))))

(defun do-all-result-series-calc (series &optional (n 20))
  "Produce a list of the best returns from all teams for each result (Win/Draw/Loss/Win-Loss/Loss-Win)"
  (do-all-series-calc series n series-results series-fns series-odds-fns))

(defun do-all-ou-series-calc (series &optional (n 20))
  "Produce a list of the best returns from all teams for each result (Over/Under/Over-Under/Under-Over)"
  (do-all-series-calc series n series-ou-results series-ou-fns series-ou-odds-fns))

(defun do-last-six-result-series-calc (series &optional (n 20))
  "Produce a list of the best returns from the last six games of all teams for each result (Win/Draw/Loss/Win-Loss/Loss-Win)"
  (do-all-series-calc series n series-results series-fns series-odds-fns #'last-six))

(defun do-last-six-ou-series-calc (series &optional (n 20))
  "Produce a list of the best returns from the last six games of all teams for each result (Over/Under/Over-Under/Under-Over)"
  (do-all-series-calc series n series-ou-results series-ou-fns series-ou-odds-fns #'last-six))

(defun do-all-result-series (series &optional (n 20))
  "Produce a list of th;e best returns from all teams for each result (Win/Draw/Loss/Win-Loss/Loss-Win)"
  (do-all-series-table (do-all-result-series-calc series n)))

;;; add home-overs, away-overs/unders etc to this - see 2696 series-ou-fns
(defun do-all-ou-series (series &optional (n 20))
  "Produce a list of the best returns from all teams for each result (Over/Under/Over-Under/Under-Over)"
  (do-all-series-table (do-all-ou-series-calc series n)))

(defun do-last-six-result-series (series &optional (n 20))
  "Produce a list of the best returns from the last six games of all teams for each result (Win/Draw/Loss/Win-Loss/Loss-Win)"
  (do-all-series-table (do-last-six-result-series-calc series n)))

(defun do-last-six-ou-series (series &optional (n 20))
  "Produce a list of the best returns from the last six games of all teams for each result (Over/Under/Over-Under/Under-Over)"
  (do-all-series-table (do-last-six-ou-series-calc series n)))

(defun write-all-ou-series (series &optional (n 30)) 
  (with-open-file (stream "c:/mine/lisp/data/all-ou-series.csv"
						  :direction :output
						  :if-exists :supersede)
	(dolist (my-list (do-all-ou-series-calc series n))
	  (format stream "~{~a~^,~}~%" my-list))
	(format stream "~%"))
  t)

(defparameter uk-series-funcs
  `(("Wins" ,#'do-series-wins-calc)
	("Home Wins" ,#'do-series-home-wins-calc)
	("Away Wins" ,#'do-series-away-wins-calc)
	("Draws" ,#'do-series-draws-calc)
	("Home Draws" ,#'do-series-home-draws-calc)
	("Away Draws" ,#'do-series-away-draws-calc)
	("Defeats" ,#'do-series-defeats-calc)
	("Home Defeats" ,#'do-series-home-defeats-calc)
	("Away Defeats",#'do-series-away-defeats-calc)
	("Overs" ,#'do-series-overs-calc)
	("Home Overs" ,#'do-series-home-overs-calc)
	("Away Overs" ,#'do-series-away-overs-calc)
	("Unders" ,#'do-series-unders-calc)
	("Home Unders" ,#'do-series-home-unders-calc)
	("Away Unders" ,#'do-series-away-unders-calc)
	("Over Unders" ,#'do-series-over-unders-calc)
	("Under Overs" ,#'do-series-under-overs-calc)
	("L6 Over Unders" ,#'do-last-six-over-unders-calc)
	("L6 Under Overs" ,#'do-last-six-under-overs-calc)
	("All Results" ,#'do-all-result-series-calc)
	("All OU Results" ,#'do-all-ou-series-calc)))

(defparameter summer-series-funcs
  `(("Wins" ,#'do-series-wins-calc)
	("Home Wins" ,#'do-series-home-wins-calc)
	("Away Wins" ,#'do-series-away-wins-calc)
	("Draws" ,#'do-series-draws-calc)
	("Home Draws" ,#'do-series-home-draws-calc)
	("Away Draws" ,#'do-series-away-draws-calc)
	("Defeats" ,#'do-series-defeats-calc)
	("Home Defeats" ,#'do-series-home-defeats-calc)
	("Away Defeats",#'do-series-away-defeats-calc)
	("Overs" ,#'do-series-overs-calc)
	("Unders" ,#'do-series-unders-calc)
	("Over Unders" ,#'do-series-over-unders-calc)
	("Under Overs" ,#'do-series-under-overs-calc)
	("All Results" ,#'do-all-result-series-calc)
	("All OU Results" ,#'do-all-ou-series-calc)))

(defun write-series (series filename n series-funcs)
  (with-open-file (stream filename
						  :direction :output
						  :if-exists :supersede)
	(dolist (series-pair series-funcs)
	  (destructuring-bind (series-result series-fn) series-pair
		(format stream "~a~%" series-result)
		(dolist (my-list (first-n n (funcall series-fn series)))
		  (format stream "~{~a~^,~}~%" my-list)))
	  (format stream "~%"))))

(defun export-all-series (n series-funcs country)
  (dolist (series-pair series-list 'done)
	(destructuring-bind (series series-name) series-pair
	  (let ((filename (format nil "c:/mine/lisp/data/series ~a ~a.csv" series-name country)))
		(format t "~%Writing ~a..." filename)
		(write-series series filename n series-funcs)))))

(defun export-series (&optional (n 30))
  (let ((country "")
		(series-funcs uk-series-funcs))
	(cond ((equal *leagues* *uk-leagues*)
		   (setf country "UK"))
		  ((equal *leagues* *euro-leagues*)
		   (setf country "Euro"))
		  (t (setf country "Summer")
			 (setf series-funcs summer-series-funcs)))
	(export-all-series n series-funcs country)))

(defun do-team-series-calc (team s games-fn result-fn odds-fn)
  "Returns details of a TEAM with series S, using games returned from GAMES-FN
   which match RESULT-FN at odds ODDS-FN"
  
  (let ((my-list nil)
		(stake 0)
		(returns 0)
		(result ""))

	(series-reset s)
	(dolist (game (funcall games-fn team))
	  (let* ((current (series-current s))
			 (odds (funcall odds-fn team game))
			 (my-stake (calc-new-stake current odds)))
		(+= stake my-stake)
		(cond ((funcall result-fn team game)
			   (+= returns (* my-stake odds))
			   (setf result "W"))
			  (t (setf result "L")))
		(series-update s result)
		(push (list (date game) (home-team game) (away-team game)
					current my-stake result
					(home-odds game) (draw-odds game) (away-odds game)
					(over-odds game) (under-odds game)
 					stake returns)
			  my-list)))

	(values (reverse my-list)
			stake
			returns)))

(defun do-team-series (team s games-list result-fn odds-fn)
  (format t "~72t1~78tX~84t2~93tO~99tU~104tStake~109tReturn")

  (multiple-value-bind (my-list stake returns)
	  (do-team-series-calc team s games-list result-fn odds-fn)

	(format t "~{~{~%~a  ~a ~30t v ~a ~55t~a ~60t~a  ~65t ~a ~5,2f ~5,2f ~5,2f  : ~5,2f ~5,2f : ~6,2f ~6,2f~}~}" my-list)
	(format t "~%~%Stake  : £~,2f~%Return : £~,2f~%Percentage : ~,2f%" stake returns (calc-percent stake returns))))

(defun do-team-series-wins (team series)
  (do-team-series team series #'home-aways #'home-away-win-result #'home-away-odds))
(defun do-team-series-home-wins (team series)
  (do-team-series team series #'homes #'home-away-win-result #'home-away-odds))
(defun do-team-series-away-wins (team series)
  (do-team-series team series #'aways #'home-away-win-result #'home-away-odds))

(defun do-team-series-defeats (team series)
  (do-team-series team series #'home-aways #'home-away-defeat-result #'home-away-lost-odds))
(defun do-team-series-home-defeats (team series)
  (do-team-series team series #'homes #'home-away-defeat-result #'home-away-lost-odds))
(defun do-team-series-away-defeats (team series)
  (do-team-series team series #'aways #'home-away-defeat-result #'home-away-lost-odds))

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

(defun do-team-over-under-series-calc (team s home-result-fn home-odds-fn away-result-fn away-odds-fn)
  "Returns details of a TEAM with series S, using games returned from GAMES-FN
   which match RESULT-FN at odds ODDS-FN"
  
  (let ((my-list nil)
		(stake 0)
		(my-stake 0)
		(returns 0)
		(result ""))

	(series-reset s)
	(dolist (game (home-aways team))
	  (let ((current (series-current s)))
		(cond ((is-home team game)
			   (let ((odds (funcall home-odds-fn team game)))
				 (setf my-stake (calc-new-stake current odds))
				 (+= stake my-stake)
				 (cond ((funcall home-result-fn team game)
						(+= returns (* my-stake odds))
						(setf result "W"))
					   (t (setf result "L")))))
			  (t (let ((odds (funcall away-odds-fn team game)))
				   (setf my-stake (calc-new-stake current odds))
				   (+= stake my-stake)
				   (cond ((funcall away-result-fn team game)
						  (+= returns (* my-stake odds))
						  (setf result "W"))
						 (t (setf result "L"))))))
		(series-update s result)
		(push (list (date game) (home-team game) (away-team game)
					current my-stake result
					(home-odds game) (draw-odds game) (away-odds game)
					(over-odds game) (under-odds game)
 					stake returns)
			  my-list)))
	(values (reverse my-list)
			stake
			returns)))

(defun do-ou-team-series (team s home-result-fn home-odds-fn away-result-fn away-odds-fn)
  (format t "~71t1~77tX~82t2~92tO~98tU~103tStake~110tReturn")

  (multiple-value-bind (my-list stake returns)
	  (do-team-over-under-series-calc team s home-result-fn home-odds-fn away-result-fn away-odds-fn)

	(format t "~{~{~%~a  ~a ~30t v ~a ~55t~a ~60t~a  ~65t~a ~5,2f ~5,2f ~5,2f  : ~5,2f ~5,2f : ~6,2f ~6,2f~}~}" my-list)
	(format t "~%~%Stake  : £~,2f~%Return : £~,2f~%Percentage : ~,2f%" stake returns (* (/ returns stake) 100))))

(defun do-team-series-over-unders (team s)
  (do-ou-team-series team s #'series-overs #'series-over-odds #'series-unders #'series-under-odds))
(defun do-team-series-under-overs (team s)
  (do-ou-team-series team s #'series-unders #'series-under-odds #'series-overs #'series-over-odds))

(defun do-team-series-win-loss (team s)
  (do-ou-team-series team s #'is-win #'series-home-odds #'is-defeat #'series-home-odds))
(defun do-team-series-loss-win (team s)
  (do-ou-team-series team s #'is-defeat #'series-away-odds #'is-win #'series-away-odds))

;; Compare returns

(defun team-series-cmp (team games-list result-fn odds-fn series)
  "Calculate returns for TEAM for given RESULT-FN"
  (multiple-value-bind (my-list stake returns)
	  (do-team-series-calc team series games-list result-fn odds-fn)
	my-list ;; need to evaluate this to avoid an error but don't want to return it below
	(values stake returns)))

(defun do-team-series-cmp (home-team away-team result-fn odds-fn series)
  "Compare home and away returns for HOME-TEAM and AWAY-TEAM for given RESULT-FN "
  (multiple-value-bind (stake returns)
	  (team-series-cmp home-team #'homes result-fn odds-fn series)
	(format t "~%~a ~20t: £~6,2f £~6,2f ~7,2f%" home-team stake returns (calc-percent stake returns)))
  (multiple-value-bind (stake returns)
	  (team-series-cmp away-team #'aways result-fn odds-fn series)
	(format t "~%~a ~20t: £~6,2f £~6,2f ~7,2f%" away-team stake returns (calc-percent stake returns))))

(defun do-team-series-overs-cmp (home-team away-team &optional (series s1))
  (do-team-series-cmp home-team away-team #'series-overs #'series-over-odds series))

(defun do-team-series-unders-cmp (home-team away-team &optional (series s1))
  (do-team-series-cmp home-team away-team #'series-unders #'series-under-odds series))

;; ****************************************

;; DSL Streaks
;;

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
			(when (> signal-result 0)
			  (+= stake current))
			(cond ((funcall result-fn team game)
				   (when (> signal-result 0)
					 (+= returns (* current (funcall odds-fn team game)))
					 (series-update s "W"))

				   (incf signal-result)
				   (when (> signal-result 3)
					 (setf signal-result 0)))
				  (t (series-update s "R")
					 (setf signal-result 0)))))
		
		(if (> stake 0)
			(push (list (string-upcase (csv-filename league))
						team
						stake
						(format nil "~6,2f" returns)
						(calc-percent stake returns))
				  my-list))))

	(sort my-list #'> :key #'fifth)))

(defun do-streak-wins-calc (series)
  (do-streak series #'home-aways #'home-away-win-result #'home-away-odds))
(defun do-streak-defeats-calc (series)
  (do-streak series #'home-aways #'home-away-defeat-result #'home-away-lost-odds))
(defun do-streak-draws-calc (series)
  (do-streak series #'home-aways #'home-away-draw-result #'series-draw-odds))
(defun do-streak-overs-calc (series)
  (do-streak series #'home-aways #'series-overs #'series-over-odds))
(defun do-streak-unders-calc (series)
  (do-streak series #'home-aways #'series-unders #'series-under-odds))

(defun do-streak-wins (series &optional (n 10))
  (series-table n (do-streak-wins-calc series)))
(defun do-streak-defeats (series &optional (n 10))
  (series-table n (do-streak-defeats-calc series)))
(defun do-streak-draws (series &optional (n 10))
  (series-table n (do-streak-draws-calc series)))
(defun do-streak-overs (series &optional (n 10))
  (series-table n (do-streak-overs-calc series)))
(defun do-streak-unders (series &optional (n 10))
  (series-table n (do-streak-unders-calc series)))

(defun do-team-streak (s team games-fn result-fn odds-fn)
  "Returns a list of all teams sorted by their returns using series S given the 
   games returned by GAMES-FN which match results from RESULT-FN at odds ODDS-FN"
  
  (let ((stake 0)
		(returns 0)
		(signal-result 0))
	(series-reset s)

	(dolist (game (funcall games-fn team))
	  (let ((current (series-current s)))
		(when (> signal-result 0)
		  (+= stake current))
		(cond ((funcall result-fn team game)
			   (cond ((> signal-result 0)
					  (+= returns (* current (funcall odds-fn team game)))
					  (series-update s "W")
					  (format t "~%W W : ~a v ~a Stake : ~a Return : ~a" (home-team game) (away-team game) stake returns))
					 (t (format t "~%X * : ~a v ~a" (home-team game) (away-team game))))
			   (incf signal-result)
			   (when (> signal-result 3)
				 (setf signal-result 0))			   )

			  (t (series-update s "R")
				 (if (> signal-result 0)
					 (format t "~%L L : ~a v ~a Stake : ~a Return : ~a" (home-team game) (away-team game) stake returns)
					 (format t "~%X   : ~a v ~a" (home-team game) (away-team game)))
				 
				 (setf signal-result 0)))))))

(defun do-team-streak-wins-calc (team series)
  (do-team-streak series team #'home-aways #'home-away-win-result #'home-away-odds))
(defun do-team-streak-defeats-calc (team series)
  (do-team-streak series team #'home-aways #'home-away-defeat-result #'home-away-lost-odds))
(defun do-team-streak-draws-calc (team series)
  (do-team-streak series team #'home-aways #'home-away-draw-result #'series-draw-odds))
(defun do-team-streak-overs-calc (team series)
  (do-team-streak series team #'home-aways #'series-overs #'series-over-odds))
(defun do-team-streak-unders-calc (team series)
  (do-team-streak series team #'home-aways #'series-unders #'series-under-odds))

(defun load-my-streak-teams ()
  (with-open-file (in "c:/mine/lisp/data/my-streak-teams.dat")
	(with-standard-io-syntax
	  (setf *streak-teams* (read in)))))

(defun save-my-streak-teams ()
  (with-open-file (out "c:/mine/lisp/data/my-streak-teams.dat"
					   :direction :output
					   :if-exists :supersede)
	(with-standard-io-syntax
	  (print *streak-teams* out))))

(defun make-signal-funcs-ht ()
  (let ((ht (make-hash-table)))
	(setf (gethash 'Wins ht) #'is-win)
	(setf (gethash 'Draws ht) #'series-is-draw)
	(setf (gethash 'Defeats ht) #'is-defeat)
	(setf (gethash 'Overs ht) #'series-overs)
	(setf (gethash 'Unders ht) #'series-unders)
	ht))

(defun check-for-signal-result (team result-fn)
  (let ((game-list (reverse (last-n (home-aways team) 5))))
	(cond ((funcall result-fn team (first game-list))
		   (or (not (funcall result-fn team (second game-list))) ;; beginning of new streak
			   (and (funcall result-fn team (third game-list))   ;; continuation of previous streak
					(funcall result-fn team (fourth game-list))
					(funcall result-fn team (fifth game-list)))))
		  (t nil))))

;; Write func to follow on from this to show fixtures for each team.
;; maybe even a following list of teams not playing;
;; copy signal-list then use mapcar teams (remove team signal-list-copy :test #'string-equal)

(defun get-signal-results ()
  (let ((signal-funcs-ht (make-signal-funcs-ht))
		(signal-list nil))
	(load-my-streak-teams)
	(mapcar #'(lambda (streak-pair)
				(destructuring-bind (team result) streak-pair
				  (when (check-for-signal-result team (gethash result signal-funcs-ht))
					(push streak-pair signal-list))))
			*streak-teams*)
	signal-list))

(defun say-signal-results ()
  (format t "~{~{~%~a ~18t~a~}~}" (get-signal-results)))

(defparameter *streak-funcs*
  `(("Wins" ,#'do-streak-wins-calc)
	("Draws" ,#'do-streak-draws-calc)
	("Defeats" ,#'do-streak-defeats-calc)
	("Overs" ,#'do-streak-overs-calc)
	("Unders" ,#'do-streak-unders-calc)))

(defparameter *summer-streak-funcs*
  `(("Wins" ,#'do-streak-wins-calc)
	("Draws" ,#'do-streak-draws-calc)
	("Defeats" ,#'do-streak-defeats-calc)))

(defun write-streaks (series funcs filename)
  (with-open-file (stream filename
						  :direction :output
						  :if-exists :supersede)
	(dolist (series-pair funcs)
	  (destructuring-bind (streak-result streak-fn) series-pair
		(format t "~%Writing Streaks - ~a" streak-result)
		(format stream "~a~%" streak-result)
		(dolist (my-list (funcall streak-fn series))
		  (format stream "~{~a~^,~}~%" my-list)))
	  (format stream "~%"))))

(defun export-streaks (series)
  (let ((filename "")
		(funcs *streak-funcs*))
	(cond ((equal *leagues* *uk-leagues*)
		   (setf filename "c:/mine/lisp/data/streaks uk.csv"))
		  ((equal *leagues* *euro-leagues*)
		   (setf filename "c:/mine/lisp/data/streaks euro.csv"))
		  (t (setf filename "c:/mine/lisp/data/streaks summer.csv")
			 (setf funcs *summer-streak-funcs*)))
	(write-streaks series funcs filename)))

(defun my-streak-teams ()
  *streak-teams*)

(defun my-streak-teams-add (&rest team-list)
  (dolist (team team-list)
	(push team *streak-teams*))
  (save-my-streak-teams))

(defun my-streak-teams-remove (&rest team-list)
  (dolist (team team-list)
	(setf *streak-teams*
		  (remove-if #'(lambda (team-pair)
						 (string-equal (first team-pair) team))
					 *streak-teams*)))
  (save-my-streak-teams))

(defun my-streak-teams-remove-all ()
  (setf *streak-teams* nil))

(defun my-streak-teams-update-all (&rest team-list)
  (my-teams-remove-all)
  (apply #'my-streak-teams-add team-list))

(defun get-streak-games ()
  (let ((my-list nil)
		(my-teams (my-streak-teams)))
	(dolist (game *fixtures*)
	  (mapcar #'(lambda (team)
				  (let ((team-name (first team)))
					(when (or (equal team-name (fhome game))
							  (equal team-name (faway game)))
					  (push game my-list))))
			  my-teams))
	my-list))

(defun do-streaks ()
  (format t "~%~%Signal results :~%")
  (say-signal-results)
  (format t "~%~%Streak games :~%")
  (get-streak-games))

(defun start++ ()
  (update-csv-files)
  (update-summer-csv-files)
  (start+)
  (export-returns)
;  (export-streaks st5)
 ; (do-streaks)
  )

;; ***************************************************************
;; DSL
;; Result percentages

(defun do-results (games-fn result-fn)
  "Returns a list of all teams sorted by percentage of results given by RESULTS-FN from GAMES-FN"
  
  (let ((my-list nil))
	(with-all-teams (team *leagues*)
	  (let ((win-count 0)
			(game-count 0))
		(dolist (game (funcall games-fn team))
		  (incf game-count)
		  (when (funcall result-fn team game)
			(incf win-count)))

		(push (list (string-upcase (csv-filename league))
					team
					win-count
					game-count
					(calc-percent game-count win-count))
			  my-list)))

	(sort my-list #'> :key #'fifth)))

(defun do-home-away-wins-calc ()
  (do-results #'home-aways #'home-away-win-result))

(defun do-home-away-over-wins-calc ()
  (do-results #'home-aways #'series-overs))
(defun do-home-over-wins-calc ()
  (do-results #'homes #'series-overs))
(defun do-away-over-wins-calc ()
  (do-results #'aways #'series-overs))

(defun do-home-away-draws-calc ()
  (do-results #'home-aways #'home-away-draw-result))
(defun do-home-draws-calc ()
  (do-results #'homes #'home-away-draw-result))
(defun do-away-draws-calc ()
  (do-results #'aways #'home-away-draw-result))

(defun do-home-away-wins (&optional (n 10))
  (series-table n (do-home-away-wins-calc)))

(defun do-home-away-overs (&optional (n 10))
  (series-table n (do-home-away-over-wins-calc)))
(defun do-home-overs (&optional (n 10))
  (series-table n (do-home-over-wins-calc)))
(defun do-away-overs (&optional (n 10))
  (series-table n (do-away-over-wins-calc)))

(defun do-home-away-draws (&optional (n 10))
  (series-table n (do-home-away-draws-calc)))
(defun do-home-draws (&optional (n 10))
  (series-table n (do-home-draws-calc)))
(defun do-away-draws (&optional (n 10))
  (series-table n (do-away-draws-calc)))

;; *******************************************
;; Max games since RESULT
;;

(defmacro update-if-gt (counter max-seen)
  `(when (> ,counter ,max-seen)
	 (setf ,max-seen ,counter)))

(defun max-games-without-draw (games-fn)
  (let ((my-list nil))
	(with-all-teams (team *leagues*)
	  (let ((count 0)
			(max 0))
		(dolist (game (funcall games-fn team))
		  (cond ((equal "D" (result game))
				 (update-if-gt count max)
				 (setf count 0))
				(t (incf count))))
		(update-if-gt count max)
		(push (list team max) my-list)))
	(sort my-list #'> :key #'second)))

(defun max-games-no-draw-list (n)
  (first-n n (max-games-without-draw #'home-aways)))
(defun max-home-games-no-draw-list (n)
  (first-n n (max-games-without-draw #'homes)))
(defun max-away-games-no-draw-list (n)
  (first-n n (max-games-without-draw #'aways)))

(defun max-games-table (my-list)
  (format-table t my-list
				:column-label '("Team" "Games")
				:column-align '(:center :center)))

(defun max-games-no-draw (&optional (n 10))
  (max-games-table (max-games-no-draw-list n)))
(defun max-home-games-no-draw (&optional (n 10))
  (max-games-table (max-home-games-no-draw-list n)))
(defun max-away-games-no-draw (&optional (n 10))
  (max-games-table (max-away-games-no-draw-list n)))

;; *******************************************
;; DSL Count results per league
;;

(defun count-league-results (result-fn &optional (n 0))
  "Find league with highest average of results as defined by RESULT-FN"
  (dolist (league *leagues*)
	(let* ((filename (csv-filename league))
		   (games (get-league filename))
		   (count 0)
		   (total-games 0))
	  (dolist (game games)
		(incf total-games)
		(when (funcall result-fn game n)
		  (incf count)))
	  (format t "~%~a ~5t: Wins : ~3d Games : ~3d Percent : ~5,2f%"
			  (string-upcase filename) count total-games (calc-percent total-games count)))))

(defun count-average-league-goals ()
  (dolist (league *leagues*)
	(let* ((filename (csv-filename league))
		   (games (get-league filename))
		   (goals 0)
		   (total-games 0))
	  (dolist (game games)
		(incf total-games)
		(incf goals (+ (home-score game) (away-score game))))
	  (format t "~%~a ~5t: ~8,4f"
			  (string-upcase filename) (/ goals total-games)))))

;; can expand these to home/away wins,
;; also favourites then home/away favourites ??
;; poss use do-season to break down favourites week-by-week

(defun league-draws (game &optional (n 0))
  (declare (ignore n)) ;; work-around to allow count-league-overs/unders to pass goals parameter to count-league-results
  (equal (result game) "D"))
(defun league-home-wins (game &optional (n 0))
  (declare (ignore n)) ;; work-around
  (equal (result game) "H"))
(defun league-away-wins (game &optional (n 0))
  (declare (ignore n)) ;; work-around
  (equal (result game) "A"))

(defun league-home-double-chance (game &optional (n 0))
  (declare (ignore n))
  (not (equal (result game) "A")))
(defun league-away-double-chance (game &optional (n 0))
  (declare (ignore n))
  (not (equal (result game) "H")))

(defmacro do-count-games (fn-name fn)
  `(defun ,fn-name (&optional (n 2.5))
	 (count-league-results ,fn n)))

(do-count-games count-league-draws #'league-draws)
(do-count-games count-league-home-wins #'league-home-wins)
(do-count-games count-league-away-wins #'league-away-wins)
(do-count-games count-league-home-double-chance #'league-home-double-chance)
(do-count-games count-league-away-double-chance #'league-away-double-chance)
(do-count-games count-league-overs #'is-over)
(do-count-games count-league-unders #'is-under)

(defun is-over-win (game)
  (and (is-over game)
	   (> (over-odds game)
		  (under-odds game))))

(defun over-under-spread ()
  "Returns count of games with higher over-odds that result in over-results"
  (dolist (league *uk-leagues*)
	(let ((filename (csv-filename league))
		  (games 0)
		  (wins 0))
	  (mapcar #'(lambda (game)
				  (incf games)
				  (if (is-over-win game)
					  (incf wins)))
			  (get-league filename))
	  (format t "~%~a ~5t: Games : ~3d Wins : ~3d Percent : ~,2f%"
			  (string-upcase filename) games wins (calc-percent games wins)))))

;; *******************************************
;; Both teams to score
;;

(defun is-btts (game)
  (and (> (home-score game) 0)
	   (> (away-score game) 0)))

(defun not-btts (game)
  (not (is-btts game)))

(defun list-btts (team)
  (remove-if-not #'(lambda (game)
					 (is-btts game))
				 (home-aways team)))

(defun list-not-btts (team)
  (remove-if-not #'(lambda (game)
					 (not-btts game))
				 (home-aways team)))

(defun btts-percent (team)
  (calc-percent (length (home-aways team))
				(length (list-btts team))))

(defun get-btts (league)
  (let ((my-list nil))
	(mapcar #'(lambda (team)
				(push (list team (btts-percent team)) my-list))
			(get-teams league))
	my-list))

(defun get-all-btts (leagues)
  (let ((my-list nil))
	(dolist (league leagues)
	  (setf my-list (append my-list (get-btts (csv-filename league)))))
	my-list))

(defun sort-btts (leagues &optional (n 10))
  (first-n n (sort (get-all-btts leagues)
				   #'> :key #'second)))

(defun say-btts (team)
  (say team (list-btts team)))

(defun say-not-btts (team)
  (say team (list-not-btts team)))

;; *******************************************
;; Handicaps
;;

(defun calc-team-spread (team games hcap)
  (let ((my-list nil)
		(handicap (* -1 (abs hcap)))) ;; ensure negative
	(mapcar #'(lambda (game)
				(when (or (and (is-home team game)
							   (> (+ (home-score game) handicap)
								  (away-score game)))
						  (and (is-away team game)
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
		(when (> wins 0)
		  (push (list team wins games (calc-percent games wins))
				my-list))))
	(format t "~{~{~%~a ~18t: ~2d  ~2d  ~5,2f %~}~}"
			(first-n n (sort my-list #'> :key #'fourth)))))

;; ***************************************************************
;; DSL
;; Analyse summer over/under data

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

(defun do-ou-wins-table (n my-list)
  (format-table t (first-n n my-list)
				:column-label '("League" "Team" "Wins")
				:column-align '(:left :center :center )))

(defun do-summer-over-wins (&optional (n 30))
  (do-ou-wins-table n
	  (get-ou-wins *summer-leagues* #'home-aways #'is-over)))
(defun do-summer-home-over-wins (&optional (n 30))
  (do-ou-wins-table n
	  (get-ou-wins *summer-leagues* #'homes #'is-over)))
(defun do-summer-away-over-wins (&optional (n 30))
  (do-ou-wins-table n
	  (get-ou-wins *summer-leagues* #'aways #'is-over)))

(defun do-summer-under-wins (&optional (n 30))
  (do-ou-wins-table n
	  (get-ou-wins *summer-leagues* #'home-aways #'is-under)))
(defun do-summer-home-under-wins (&optional (n 30))
  (do-ou-wins-table n
	  (get-ou-wins *summer-leagues* #'homes #'is-under)))
(defun do-summer-away-under-wins (&optional (n 30))
  (do-ou-wins-table n
	  (get-ou-wins *summer-leagues* #'aways #'is-under)))

(defun do-ou-wins-by-league (csv-league games-fn result-fn)
  (let ((league-as-list `((,csv-league ""))))  ;; league-name not required here
	(do-ou-wins-table (length (get-teams csv-league))
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

;; ***************************************************************
;; DSL
;; Favourites

(defun do-favourites (&optional (my-stake 1))
  "Bet on underdogs for all games"
  (dolist (league *leagues*)
	(let ((stake 0)
		  (fav-wins 0)
		  (under-wins 0)
		  (draw-wins 0))
	  (dolist (game (get-league (csv-filename league)))
		(+= stake my-stake)
		(cond ((string-equal (result game) "D")
			   (+= draw-wins (* my-stake (draw-odds game))))
			  ((< (home-odds game)
				  (away-odds game)) ; home favourite
			   (if (string-equal (result game) "H")
				   (+= fav-wins (* my-stake (home-odds game)))
				   (+= under-wins (* my-stake (away-odds game)))))
			  ((< (away-odds game)
				  (home-odds game)) ; home favourite
			   (if (string-equal (result game) "A")
				   (+= fav-wins (* my-stake (away-odds game)))
				   (+= under-wins (* my-stake (home-odds game)))))
			  (t (-= stake my-stake)))) ; odds are equal - no play

	  (format t "~%~% ~a " (csv-league-name league))
	  (format t "~% Stake : £~,2f ~20t Underdogs  : £~,2f (~,2f%) ~55t Favourites : £~,2f (~,2f%) ~92t Draws : £~,2f (~,2f%)"
			  stake
			  under-wins (calc-percent stake under-wins)
			  fav-wins (calc-percent stake fav-wins)
			  draw-wins (calc-percent stake draw-wins)))))


(defun do-favourites2 (&key (my-stake 1) (udog-odds 2.8))
  "Bet on underdogs for all games where underdog 1X2 odds <= UDOG-ODDS - not much use because 1X2 odds will be much higher than DNB"
  (dolist (league *leagues*)
	(let ((stake 0)
		  (fav-wins 0)
		  (under-wins 0)
		  (draw-wins 0))
	  (dolist (game (get-league (csv-filename league)))
		(+= stake my-stake)
		(cond ((string-equal (result game) "D")
			   (+= draw-wins (* my-stake (draw-odds game))))
			  ((and (< (home-odds game) ; home favourites
					   (away-odds game))
					(<= (away-odds game) udog-odds))
			   (if (string-equal (result game) "H")
				   (+= fav-wins (* my-stake (home-odds game)))
				   (+= under-wins (* my-stake (away-odds game)))))
			  ((and (< (away-odds game) ; away favourites
					   (home-odds game))
					(<= (home-odds game) udog-odds))
			   (if (string-equal (result game) "A")
				   (+= fav-wins (* my-stake (away-odds game)))
				   (+= under-wins (* my-stake (home-odds game)))))
			  (t (-= stake my-stake)))) ; odds are equal - no play

	  (format t "~%~% ~a " (csv-league-name league))
	  (format t "~% Stake : £~,2f ~20t Underdogs  : £~,2f (~,2f%) ~55t Favourites : £~,2f (~,2f%) ~92t Draws : £~,2f (~,2f%)"
			  stake
			  under-wins (calc-percent stake under-wins)
			  fav-wins (calc-percent stake fav-wins)
			  draw-wins (calc-percent stake draw-wins)))))

(defun fav-win (game)
  (or (and (< (home-odds game) (away-odds game))
		   (string-equal (result game) "H"))
	  (and (< (away-odds game) (home-odds game))
		   (string-equal (result game) "A"))))

(defun udog-win (game)
  (or (and (< (home-odds game) (away-odds game))
		   (string-equal (result game) "A"))
	  (and (< (away-odds game) (home-odds game))
		   (string-equal (result game) "H"))))

(defun fav-under-wins-by-league ()
  (labels ((inner (league fav-wins udog-wins games)
			 (mapcar #'(lambda (game)
						 (incf games)
						 (cond ((fav-win game)
								(incf fav-wins))
							   ((udog-win game)
								(incf udog-wins))
							   (t )))
					 (get-league (csv-filename league)))
			 (format t "~%~%~a :"  (csv-league-name league))
			 (format t "~%Games : ~a" games)
			 (format t "~20t Favourite Wins : ~a (~,2f%)" fav-wins (calc-percent games fav-wins))
			 (format t "~54t Underdog Wins  : ~a (~,2f%)" udog-wins (calc-percent games udog-wins))))

	(dolist (league *leagues*)
	  (inner league 0 0 0))))

;; ***************************************************************
;; DSL
;; Load historical files

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

(defun load-archive-league-data (league)
  (list `(,(csv-filename league) .
		  ,(import-csv
			(format nil "c:/mine/lisp/data/historical/~a.csv" (csv-filename league))))))

(defun update-historical-files (leagues year from-path to-path &optional (csv-cols *historical-csv-cols*))
  "Transform updated CSV files to required format"
  (dolist (league leagues)
    (let ((from-str (format nil "~a/~a/~a.csv" from-path (csv-league-name league) year))
          (to-str (format nil "~a/~a.csv" to-path (csv-filename league))))
      (format t "~%Writing ~a" to-str)
      (transform-csv from-str to-str csv-cols))))

(defun load-historical (year &optional (leagues *leagues*))
  (let? csv-cols (< year 2019)
	  *historical-csv-cols*
	  *historical-csv-cols-2019+*
	(update-historical-files leagues year
							 "c:/mine/perl/football/data/historical"
							 "c:/mine/lisp/data/historical"
							 csv-cols)))

(defun load-archive-leagues (year)
  (setf *db* nil)
  (load-historical year)

  (dolist (league *leagues*)
	(setf *db* (append *db* (load-archive-league-data league))))
  (setf *teams* (get-historical-teams)))

(defun start-archive (year)
  "Load all data"
  (clear-database)
  (load-archive-leagues year)
  t)

(defun do-euro-series-calc (series)
  "Returns stake and return for list of games at Euro 2021 using series S"

  (let ((stake 0)
		(returns 0))
	(series-reset series)
	(dolist (game (import-csv "C:/Mine/perl/Football/data/euros.csv"))
	  (let ((current (series-current series)))
		(+= stake current)
		(when (string-equal (second game) "W")
		  (+= returns (* current (parse-float (first game)))))
		(series-update series (second game))))
	
	(values stake
			returns
			(calc-percent stake returns))))

(defun export-all ()
  (let ((switch-funcs `(("UK" ,#'switch-uk)
						("Euro" ,#'switch-euro)
						("Summer" ,#'switch-summer)
						)))
	(dolist (switch-pair switch-funcs)
	  (destructuring-bind (country switch-fn) switch-pair
		(format t "~%~%Switching to ~a~%..." country)
		(funcall switch-fn :odds nil)
		(export-returns)
		(export-series)
;		(export-streaks s2)
		))))

;; ***************************************************************
;; DSL
;; DSL Matched Betting

(defun bet-return (stake odds)
  (* stake odds))

(defun bet-profit (stake odds)
  (* stake (1- odds)))

(defun lay-bet-liability (stake odds)
  (bet-profit stake odds))

(defun lay-bet-return (lay-stake &optional (commission 0.02))
  (* lay-stake (- 1 commission)))

(defun qualifier-return (lay-return stake)
  (- lay-return stake))

(defun matched-bet-returns (lay-stake liability lay-return profit &optional (lay-bet-profit lay-return))
  (format t "~% Lay Stake        £~,2f" lay-stake)
  (format t "~% Liability        £~,2f~%" liability)
  (format t "~% Free Bet return  £~,2f" profit)
  (format t "~% Lay Bet return   £~,2f~%" lay-return)
  (format t "~% Free Bet Profit  £~,2f" (- profit liability))
  (format t "~% Lay Bet Profit   £~,2f" lay-bet-profit))

(defun free-bet-qualifier (stake odds lay-odds &optional (commission 0.02))
  (let* ((lay-stake (/ (* stake odds)
					   (- lay-odds commission)))
		 (liability (lay-bet-liability lay-stake lay-odds))
		 (lay-return (lay-bet-return lay-stake))
		 (profit (bet-profit stake odds)))

	(matched-bet-returns lay-stake liability lay-return profit (qualifier-return lay-return stake))))

;; free-bet-stake-not-returned 
(defun free-bet-snr (stake odds lay-odds &optional (commission 0.02))
  (let* ((lay-stake (/ (* stake (1- odds))
					   (- lay-odds commission)))
		 (liability (lay-bet-liability lay-stake lay-odds))
		 (lay-return (lay-bet-return lay-stake))
		 (profit (bet-profit stake odds)))
	
	(matched-bet-returns lay-stake liability lay-return profit)))

;; free-bet-stake-returned 
(defun free-bet-sr (stake odds lay-odds &optional (commission 0.02))
  (let* ((lay-stake (/ (* stake odds)
					   (- lay-odds commission)))
		 (liability (lay-bet-liability lay-stake lay-odds))
		 (lay-return (lay-bet-return lay-stake))
		 (profit (bet-return stake odds)))

	(matched-bet-returns lay-stake liability lay-return profit)))

(defun 2-ups (stake odds lay-odds back-odds &optional (commission 0.02))
  (let* ((lay-stake (/ (* stake odds)
					   (- lay-odds commission)))
		 (liability (lay-bet-liability lay-stake lay-odds))
		 (lay-return (lay-bet-return lay-stake))
		 (profit (bet-profit stake odds)))

	(format t "~% Lay Stake        £~,2f" lay-stake)
	(format t "~% Liability        £~,2f~%" liability)
	(format t "~% Qualifying Loss  £~,2f" (qualifier-return lay-return stake))
	(format t "~% Free Bet return  £~,2f" profit)
	(format t "~% Lay Bet return   £~,2f~%" lay-return)

	(format t "~% Back Stake       £~,2f" (/ (* lay-stake lay-odds) back-odds))
	(format t "~% Free Bet Profit  £~,2f" (- profit liability))))

;; ***************************************************************
;; DSL
;; Cricket
;; To copy to cl-repl on phone, copy to lisp/zap-phone.lisp, then to Dropbox/org/cl-repl.org

;; Helper functions

(defun rpo-from-deliveries (score delivs)
  (* 6 (/ score delivs)))

;; (split-deliveries 5.3) returns (values 5 3)

(defun split-deliveries (overs)
  (multiple-value-bind (ovs delivs) (floor overs)
	(values ovs (round (* 10 delivs)))))

;; (get-deliveries 5.3) returns 33

(defun get-deliveries (overs)
  (multiple-value-bind (ovs delivs) (split-deliveries overs)
	(+ (* 6 ovs)
	   delivs)))

(defun runs-per-ball (runs-per-over)
  (/ runs-per-over 6))

(defun calc-runline (score overs runline-overs)
  (* (get-deliveries runline-overs)
	 (runs-per-ball (rpo-from-deliveries score (get-deliveries overs)))))

;; Find runs per over from either full or part overs (eg 5.3 overs)

(defun get-rpo (score overs)
  (rpo-from-deliveries score (get-deliveries overs)))

(defun rpo (score overs)
 (format t "~,2f" (get-rpo score overs)))

(defun overs-left (overs-gone &optional (overs 20))
  (multiple-value-bind (ovs balls) (floor overs-gone)
	(declare (ignore ovs))
	(if (= balls 0)
		(- overs overs-gone)
		(my-round (- overs overs-gone 0.4) 0.1))))

(defun rpo-stats (chasing-score chasing-overs target &optional (overs 20))
  (format t "~%Original rpo : ~5,2f" (/ target overs))
  (format t "~%Current  rpo : ~5,2f" (get-rpo chasing-score chasing-overs))
  (format t "~%Required rpo : ~5,2f" (get-rpo (- target chasing-score)
											  (overs-left (- overs chasing-overs)))))
(defun projected-score ())

;; Calculate predicted runline amount

(defun runline (score overs runline-overs)
  (format t "~,2f" (calc-runline score overs runline-overs)))

;; Find the equivalent runs-per-6 ball over score for a hundred score

(defun hundred (score &optional (delivs 100))
  (format t "~,2f" (rpo-from-deliveries score delivs)))

;; ***************************************************************

(defparameter *dnb* nil)

(defun load-dnb ()
  (setf *dnb* (import-csv "c:/mine/perl/football/data/dnb.csv")))


