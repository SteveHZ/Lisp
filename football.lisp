
;; football.lisp May 2020

(defpackage :football
  (:use :cl :cl-csv :iterate :parse-float :ppcre
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
(defvar *historical-csv-cols* '("Date" "HomeTeam" "AwayTeam" "FTHG" "FTAG" "FTR" "B365H" "B365D" "B365A"))

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

(defparameter *summer-leagues*
  '(("swe" "Swedish League")
	("nor" "Norwegian League")
	("fin" "Finnish League")
	("roi" "Irish League")
;	("mls" "USA League")
	))

(defvar *uk-fixtures-file* "c:/mine/perl/football/data/fixtures.csv")
(defvar *summer-fixtures-file* "c:/mine/perl/football/data/summer/fixtures.csv")
(defvar *uk-teams-file* "c:/mine/lisp/data/uk-teams.dat")
(defvar *summer-teams-file* "c:/mine/lisp/data/summer-teams.dat")

;;(defparameter *leagues* *uk-leagues*)
;;(defparameter *fixtures-file* *uk-fixtures-file*)
;;(defparameter *csv-cols* *uk-csv-cols*)
;;(defparameter *teams-file* *uk-teams-file*)

(defparameter *leagues* *summer-leagues*)
(defparameter *fixtures-file* *summer-fixtures-file*)
(defparameter *csv-cols* *summer-csv-cols*)
(defparameter *teams-file* *summer-teams-file*)
;; (defparameter *leagues* (append *uk-leagues* *summer-leagues*))

(defvar *ht-stats* (make-hash-table :test #'equal))
(defvar *ht-league-stats* (make-hash-table :test #'equal))
(defvar *expects-db* (make-hash-table :test #'equal))
(defvar *my-teams*)

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
  (home-games 0)
  (away-games 0))

(defstruct (league-stats (:print-function print-league-stats))
  "Holds the total games and home/away goals for each league"
  (home-goals 0) (away-goals 0) (games 0) (av-home-goals 0) (av-away-goals 0))

(defstruct (game (:print-function print-game))
  "Holds the goal expectancy values for each current game"
  league home-team away-team home-goals away-goals goal-diff home-odds draw-odds away-odds)

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
    (format t "~% ~a : ~a v ~a ~40t~5,2f  ~5,2f  ~5,2f"
            (string-upcase (game-league x))
            (game-home-team x) (game-away-team x)
            (game-home-odds x) (game-draw-odds x)
            (game-away-odds x))))


;; ***************************************************************

;; DSL
;; Accessor macros and functions for each game in *db*
;;

(defun say-game (game)
;;  (format t "~%~{~a ~10t~a ~30t v ~a ~54t~a-~a ~a ~5*~}" game) ;; uk
  (format t "~%~{~a ~10t~a ~30t v ~a ~54t~a-~a ~a ~3*~}" game))  ;; summer

(defun say-game-odds (game)
;;  (format t "~%~{~a ~10t~a ~30t  v ~a ~54t~a-~a  ~a ~62t~a ~68t~a ~74t~a   ~84t~a ~92t~a~}" game) ;;uk
  (format t "~%~{~a ~10t~a ~30t  v ~a ~54t~a-~a  ~a ~62t~a ~68t~a ~74t~a~}" game))  ;; summer

(defun say (&optional (games *db*))
  (mapcar #'(lambda (game)
			  (say-game game))
            games))

(defun say-odds (&optional (games *db*))
  (mapcar #'(lambda (game)
			  (say-game-odds game))
          games))

(defun date (game) (first game))
(defun home-team (game) (second game))
(defun away-team (game) (third game))
(defun home-score (game) (parse-integer (fourth game)))
(defun away-score (game) (parse-integer (fifth game)))
(defun result (game) (sixth game))

(defun home-odds (game) (parse-float (seventh game)))
(defun away-odds (game) (parse-float (ninth game)))
(defun draw-odds (game) (parse-float (eighth game)))
(defun over-odds (game) (parse-float (tenth game)))
(defun under-odds (game) (parse-float (nth 10 game)))

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

(defun get-league (csv-league &optional (db *db*))
  "Returns a list of all games for given CSV-LEAGUE"
  (cond ((null db) nil)
        ((string-equal csv-league (caar db))
         (cdar db))
        (t (get-league csv-league (rest db)))))

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

(defun say-homes (team)
  (say (homes team)))
(defun say-aways (team)
  (say (aways team)))
(defun say-home-aways (team)
  (say (home-aways team)))

(defun get-last-six (fn team ngames)
  (let* ((my-data (funcall fn team))
		 (len (length my-data))
		 (start-elem (if (>= ngames len)
						 0 (- len ngames))))
	(nthcdr start-elem my-data)))

(defun last-six-homes (team &optional (ngames 6))
  (get-last-six #'homes team ngames))
(defun last-six-aways (team &optional (ngames 6))
  (get-last-six #'aways team ngames))
(defun last-six (team &optional (ngames 6))
  (get-last-six #'home-aways team ngames))

(defun last-n-homes (n team)
  (last-six-homes team n))
(defun last-n-aways (n team)
  (last-six-aways team n))
(defun last-n-games (n team)
  (last-six team n))

(defun first-n (n my-list)
  (let* ((len (length my-list))
         (start-elem (if (> len n)
                         (- len n) 0)))
    (butlast my-list start-elem)))

(defun get-results (result lst)
  (remove-if-not
   #'(lambda (game)
       (equal (result game) result))
   lst))

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

(defun get-over-unders (gt-lt-fn fn team n)
  "Returns a list of games either over or under (specified by GT-LT-FN)
   N goals for TEAM from a list of home/away/both games specified by FN"
  (remove-if-not #'(lambda (game)
					 (funcall gt-lt-fn
							  (+ (home-score game)
								 (away-score game))
							  n))
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

(defun count-league-results ()
  (mapcar #'(lambda (league)
			  (let* ((league-name (string-upcase (car league)))
					 (games (get-league league-name))
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

(defun home-away-draw-record (team)
  (destructuring-bind (home away draw)
	  (lengths (list (homes team)
					 (aways team)
					 (draws team)))
	(format t "~%Home Wins : ~a~%Away Wins : ~a~%Draws : ~a" home away draw)))

;; ***************************************************************

;; DSL
;; Accessor macros and functions for structures
;; Find stats within *ht-stats* hash for TEAM in LEAGUE
;;
;; Enables writing (get-home-for "Stoke" "e1)
;; rather than (stats-home-for (gethash "Stoke" (gethash "e1" *ht-stats*)))
;;

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

(defun get-home-games (team hash) (get-value stats-home-games team hash))
(defun get-away-games (team hash) (get-value stats-away-games team hash ))

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

;;
;; Utilities
;;

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

(defun load-leagues ()
  "Load all CSV files for leagues in *leagues*"
  (setf *db* nil)
  (dolist (lg *leagues*)
    (setf *db* (append *db* (list `(,(car lg) .
									,(import-csv (format nil "c:/mine/lisp/data/~a.csv" (car lg)))))))))

(defun load-fixtures ()
  (setf *fixtures* (import-csv *fixtures-file*)))

(defun show-fixtures ()
  "Shows current fixtures file, will return NIL if not loaded"
  (when (null *fixtures*)
	(return-from show-fixtures nil))
  (dolist (game *fixtures*)
    (format t "~%~a - ~a v ~a" (string-upcase (fleague game)) (fhome game) (faway game))))

(defun get-teams (csv-league &optional (teams *teams*))
  "Returns a list of teams in the given CSV-LEAGUE"
  (cond ((null teams) nil)
        ((string-equal csv-league (caar teams))
		 (cadar teams)) ;; returning just cdar returns 2d list ((..))
        (t (get-teams csv-league (rest teams)))))

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
               (format t "~%~%League : ~a" (cadr (assoc key *leagues*)))
               (show-hash value))
           hash))

(defun show-stats (csv-league)
  "Prints out stats for given CSV-LEAGUE"
  (show-hash (gethash csv-league *ht-stats*)))

(defun show-av-league-goals ()
  "Show average goals stats for each league"
  (format-table t (mapcar #'(lambda (league)
							  (list (cadr league)
									(format nil "~,2f" (get-league-stats-av-home-goals (car league)))
									(format nil "~,2f" (get-league-stats-av-away-goals (car league)))))
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

(defun my-teams-add (team)
  (push team *my-teams*)
  (save-my-teams))

(defun my-teams-remove (team)
  (setf *my-teams*
		(remove-if #'(lambda (tm)
					   (string-equal tm team))
				   *my-teams*))
  (save-my-teams))

(defun my-fixtures ()
  (format-table 
   t (remove-if-not #'(lambda (game)
						(or (member (fhome game) *my-teams* :test #'equal)
							(member (faway game) *my-teams* :test #'equal)))
					*fixtures*)
   :column-label '("Date" "League" "Home" "Away")
   :column-align '(:center :center :center :center)))

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

(defun last-six-draws-percentage-return (team)
  (let ((len (length (last-six team))))
	(if (= len 0) 0
		(/ (last-six-draws-return team) len))))

;; Returns per league

(defun percents-table (my-list)
  (format-table t my-list
				:column-label '("League" "Team" "Return")
				:column-align '(:left :center :center)))

(defun league-percents (fn league)
  "Calculate (loss) percentage returns for each TEAM in LEAGUE"
  (mapcar #'(lambda (team)
			  (list (string-upcase league) team
					(my-round (funcall fn team) 0.0001)))
          (get-teams league)))

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

(defun do-last-six-draw-percents (league)
  (do-league-percents #'last-six-draws-percentage-return league))

(defun percents-all (fn)
  "Calculate percentage returns for all teams in all leagues"
  (let ((my-list nil))
    (dolist (league *leagues*)
      (dolist (team (league-percents fn (car league)))
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

;; ***************************************************************

;;
;; Odds utilities
;; Convert odds between different formats

(defun decimal-to-percent (odds)
  (* 100 (/ 1.0 odds)))

(defun percent-to-decimal (pc)
  (/ 100.0 pc))

(defun frac-to-decimal (frac)
  (let ((nums (ppcre:split "-" frac)))
	(+ 1.0 (/ (parse-integer (first nums))
			  (parse-integer (second nums))))))

(defun frac-to-percent (pc)
  (decimal-to-percent
   (frac-to-decimal pc)))

(defun approx-equal (a b &optional (diff 0.1))
  "Helper function for decimal-to-frac to counter floating-point errors,
  amount of error allowed can be amended by DIFF"
  (if (<= (abs (- a b)) diff)
	  't 'nil))

(defun decimal-to-frac (dec)
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

(defun calc-new-stake-first (ds)
  "Calculate stake for system bet if odds are under 2.1"
  (let ((amount 1))
	(my-while (< (* amount ds) 2.1)
	  (+= amount 0.1))
	(format t "£~,2f" amount)))

(defun calc-new-stake (stake ds1 ds2)
  "Calculate stake for system (double) bet if odds are under 2.1"
  (let ((amount 1))
	(my-while (< (* amount ds1 ds2) (* 2.1 2.1))
	  (+= amount 0.1))
	(format t "£~,2f" (* stake amount))))

(defun next-stake (total-stake odds)
  "Calculate next stake to return a profit on a losing run"
  (let ((new-stake 1))
	(my-while (<= (* new-stake odds)
				  (+ total-stake new-stake))
	  (incf new-stake))
	new-stake))

;; ***************************************************************

;;
;; CSV file utilities
;;

(defun select-cols-from-row (wanted-list src-list)
  "select only columns in WANTED-LIST from each row of SRC-LIST,
   returns a list of the values from each wanted column"

  (let ((obj-list nil)
        (count 0))
    (labels ((inner (wanted src)
               (cond ((null wanted) (reverse obj-list))
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
               (cond ((null wanted) (reverse new-list))
                     ((string-equal (car wanted) (car src))
                      (push count new-list)
                      (incf count)
                      (inner (rest wanted) (rest src)))
                     (t (incf count)
                        (inner wanted (rest src))))))
      (inner wanted-list src-list))))

(defun data-clean (cell)
  (let ((temp (ppcre:regex-replace "'" cell "")))  ; remove apostrophe in Nott'm Forest
    (ppcre:regex-replace "^$" temp "1")))          ; fill in any blank cells

(defun transform-csv (file-from file-to columns)
  "Convert csv file FILE-FROM to new file FILE-TO only showing required columns from COLUMNS
   which can be either of the global variables *csv-cols* or *csv-summer-cols* without header line"

  (let* ((data (import-csv file-from))
         (cols-list (find-header-columns columns (car data))))
    (export-csv
     (mapcar #'(lambda (row)
                 (mapcar #'(lambda (cell)
                             (format nil "~a" (data-clean cell)))
                         (select-cols-from-row cols-list row)))
             (cdr data)) ;; remove header line
     file-to)))

(defun update-files (leagues from-path to-path)
  "Transform updated CSV files to required format"
  (dolist (league leagues)
    (let ((from-str (format nil "~a/~a.csv" from-path (car league)))
          (to-str (format nil "~a/~a.csv" to-path (car league))))
      (format t "~%Writing ~a" to-str)
      (transform-csv from-str to-str *csv-cols*))))

(defun update-csv-files ()
  (update-files *uk-leagues*
				"c:/mine/perl/football/data"
				"c:/mine/lisp/data"))

(defun update-summer-csv-files ()
  (update-files *summer-leagues*
				"c:/mine/perl/football/data/summer"
				"c:/mine/lisp/data"))

(defun update-historical-files (leagues year from-path to-path &optional (csv-cols *historical-csv-cols*))
  "Transform updated CSV files to required format"
  (dolist (league leagues)
    (let ((from-str (format nil "~a/~a/~a.csv" from-path (cadr league) year))
          (to-str (format nil "~a/~a.csv" to-path (car league))))
(format t "~%Reading ~a" from-str)
      (format t "~%Writing ~a" to-str)
      (transform-csv from-str to-str csv-cols))))

(defun load-historical (leagues year)
  (update-historical-files leagues year
						   "c:/mine/perl/football/data/historical"
						   "c:/mine/lisp/data/historical"))

(defun transform-date (d)
  "Transform date from DD/MM/YYYY to YYYYMMDD"
  (let ((x (ppcre:split "/" d)))
    (list (third x) (second x) (first x))))

(defun integer-date (date)
  "Transform date from DD/MM/YYYY to integer YYYYMMDD"
  (parse-integer (format nil "~{~a~}" (transform-date date))))

; **************************************************************

;;
;; Expects
;;

(defun build-stats (league-name league-hash)
  "Build stats for each LEAGUE-NAME, build in LEAGUE-HASH, called from do-stats in start-up"
  (dolist (team (get-teams league-name))
    (setf (gethash team league-hash) (make-stats)))
  (setf (gethash league-name *ht-league-stats*) (make-league-stats)))

(defun update-team-stats (hash game)
  "Used to update *ht-stats* structure during start-up"
    (let ((home-tm (home-team game))
          (away-tm (away-team game))
          (home-score (home-score game))
          (away-score (away-score game)))
      (set-home-for home-tm hash home-score)
      (set-home-ag  home-tm hash away-score)
      (set-away-for away-tm hash away-score)
      (set-away-ag  away-tm hash home-score)
      (incf (stats-home-games (gethash home-tm hash)))
      (incf (stats-away-games (gethash away-tm hash)))))

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
          (league-name (car league)))
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
        (let ((home-games (get-home-games team league-hash))
              (away-games (get-away-games team league-hash)))
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

(defun sort-expects ()
  (do-expects)
  (setf *sorted* (safe-sort *expects*
                   #'> :key #'game-goal-diff)))
(defun do-odds ()
  "Calculates odds for each game from goal expectancy values"
  (dolist (game *expects*)
    (let ((ds (make-instance 'my-odds :size 10)))
      (calc-game ds (game-home-goals game) (game-away-goals game))
	  (setf (values (game-home-odds game)
					(game-draw-odds game)
					(game-away-odds game))
			(vget-odds ds))))
  
  (format t "~%")
  (print-game-odds (safe-sort *expects*
                     #'< :key #'game-draw-odds)))

(defun start ()
  "Load all data"
  (clear-database)
  (import-teams)
  (load-leagues)
  (load-my-teams)
  (do-stats)
  t)

(defun start+ ()
  "Load all data, calculate goal expects and odds"
  (start)
  (calc-expects)
  (sort-expects)
  (do-odds)
  t)

(defun switch-uk ()
  (defparameter *leagues* *uk-leagues*)
  (defparameter *fixtures-file* *uk-fixtures-file*)
  (defparameter *csv-cols* *uk-csv-cols*)
  (defparameter *teams-file* *uk-teams-file*)
  (start))

(defun switch-summer ()
  (defparameter *leagues* *summer-leagues*)
  (defparameter *fixtures-file* *summer-fixtures-file*)
  (defparameter *csv-cols* *summer-csv-cols*)
  (defparameter *teams-file* *summer-teams-file*)
  (start))

;;;*******************************************

;;;
;;; Date routines
;;;

(defvar *months*
  '((01 31)
    (02 28)
    (03 31)
    (04 30)
    (05 31)
    (06 30)
    (07 31)
    (08 31)
    (09 30)
    (10 31)
    (11 30)
    (12 31)))

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
    (cond ((> d 6) (- d 7))
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

;;;*******************************************

;;;
;;; Season stats
;;;

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
                     (t  ; must be a new week
                      (push (reverse week-list) season-list)
                      (cond ((null (cdr my-list)) ; only one game for this week at the very end of the list
                             (push (car my-list) week-list)
                             (inner (rest my-list))) ; go to null list above and exit
                            (t ; start new week
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
    (do-season-expects (car league)))
  (show-ordered-expects))


;;;*******************************************

;;;
;;; Series stats
;;;

(defmacro with-all-teams ((team leagues) &body body)
  `(dolist (league ,leagues) ;;exposes league for capture by macro user
	 (dolist (,team (get-teams (car league)))
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
			(push (list team wins) my-list))))
	  (sort my-list #'> :key #'second))))

(defun unbeaten-table (my-list)
  (format-table t my-list
				:column-label '("League" "Games")
				:column-align '(:center :center)))

(defun unbeaten-home-aways ()
  (unbeaten-table
   (consecutive-games #'home-aways
					  #'(lambda (team game)
						  (home-away-win-result team game)))))

(defun since-last-win ()
  (unbeaten-table
   (consecutive-games #'home-aways
					  #'(lambda (team game)
						  (not-home-away-win-result team game)))))

(defun since-last-home-win ()
  (unbeaten-table
   (consecutive-games #'homes
					  #'(lambda (team game)
						  (declare (ignore team))
						  (string-ne (result game) "H")))))

(defun since-last-away-win ()
  (unbeaten-table
   (consecutive-games #'aways
					  #'(lambda (team game)
						  (declare (ignore team))
						  (string-ne (result game) "A")))))

(defun since-last-draw (&optional (n 10))
  (unbeaten-table
   (first-n n
			(consecutive-games #'home-aways
							   #'(lambda (team game)
								   (declare (ignore team)) ; work-around
								   (string-ne (result game) "D"))))))

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

(defun count-season (fn test-fn)
  "Returns a list of all teams sorted by the games returned by FN
   which match the result TEST-FN"

  (labels ((inner (games count)
			 (cond ((null games) count)
				   ((funcall test-fn (car games))
					(inner (rest games) (1+ count)))
				   (t (inner (rest games) count)))))

	(let ((my-list nil))
	  (with-all-teams (team *leagues*)
		(let* ((games (funcall fn team))
			   (count (inner games 0)))
 		  (push (list team
 					  (length games)
					  count
					  (my-round (* 100 (/ count (length games))) 0.01))
				my-list)))

	  (sort my-list #'> :key #'fourth))))

(defun count-games-table (my-list)
  (format-table t my-list
				:column-label '("Team" "Games" "Wins" "Percents")
				:column-align '(:left :center :center :center)))

(defun count-games (fn result n)
  (count-games-table
   (first-n n (count-season
			   fn #'(lambda (game)
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

(defun overs (game &optional (n 2.5))
  (< n (+ (home-score game)
		  (away-score game))))

(defun unders (game &optional (n 2.5))
  (not (overs game n)))

(defun count-ou (fn test-fn n goals)
  (count-games-table
   (first-n n (count-season
			   fn #'(lambda (game)
					  (funcall test-fn game goals))))))

(defun count-overs (&key (show 10) (goals 2.5))
  (count-ou #'home-aways #'overs show goals))
(defun count-home-overs (&key (show 10) (goals 2.5))
  (count-ou #'homes #'overs show goals))
(defun count-away-overs (&key (show 10) (goals 2.5))
  (count-ou #'aways #'overs show goals))

(defun count-unders (&key (show 10) (goals 2.5))
  (count-ou #'home-aways #'unders show goals))
(defun count-home-unders (&key (show 10) (goals 2.5))
  (count-ou #'homes #'unders show goals))
(defun count-away-unders (&key (show 10) (goals 2.5))
  (count-ou #'aways #'unders show goals))

;;;*******************************************

;;;
;;; Series
;;;

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

(defun series-test (series)
  (let ((in ""))
	(print (funcall series "R"))
	(my-while (string-ne "X" (input in ">"))
	  (print (funcall series in)))))

(defparameter old-s1 (make-old-circular-series '(1 2 3 4 5 6) '(5 4 3 4 5 6)))
(defparameter old-s2 (make-old-circular-series '(1 2 3 5 8 12) '(15 15)))
(defparameter old-s3 (make-old-circular-series '(1 2 3 5) '(5 5)))
(defparameter old-s4 (make-old-circular-series '(1 1) '(1 1)))

(defparameter s1 (make-short-series '(1 2 2 3 3 5 5)))
(defparameter s2 (make-short-series '(2 2 3 3 5 5 7 7)))

(defparameter s3 (make-long-series '(2 2 2 3 3 3 5 5 5 8 8 8)))
(defparameter s4 (make-long-series '(2 2 4 4 4 6 6 6 8 8 8)))
(defparameter s5 (make-circular-series '(1 2 2 3 3 5 5) '(3 4 5)))

(defun series-current (series)
  (funcall series))
(defun series-update (series result)
  (funcall series result))
(defun series-reset (series)
  (funcall series "R"))
(defun series-print (series)
  (funcall series "P"))

;; Workarounds for do-series
(defun series-odds (fn team game)
  (declare (ignore team))
  (funcall fn game))

(defun series-draw-odds (team game)
  (series-odds #'draw-odds team game))
(defun series-over-odds (team game)
  (series-odds #'over-odds team game))
(defun series-under-odds (team game)
  (series-odds #'under-odds team game))
(defun series-overs (team game)
  (series-odds #'overs team game))
(defun series-unders (team game)
  (series-odds #'unders team game))

(defun do-series (s games-fn result-fn odds-fn)
  "Returns a list of all teams sorted by the returns using series S given the 
   games returned by GAMES-FN which match results from RESULT-FN at odds ODDS-FN"
  
  (let ((my-list nil))
	(with-all-teams (team *leagues*)
	  (let ((stake 0)
			(returns 0))
		(series-reset s)
		(dolist (game (funcall games-fn team))
		  (let ((current (series-current s)))
			(+= stake current)
			(cond ((funcall result-fn team game)
				   (+= returns (* current (funcall odds-fn team game)))
				   (series-update s "W"))
				  (t (series-update s "L")))))
		(push (list (string-upcase (car league))
					team stake
					(format nil "~6,2f" returns)
					(my-round (* 100 (/ returns stake)) 0.01))
			  my-list)))
	(sort my-list #'> :key #'fifth)))

(defun do-series-wins-list (series &optional (n 30))
  (first-n n (do-series series #'home-aways #'home-away-win-result #'home-away-odds)))
(defun do-series-home-wins-list (series &optional (n 30))
  (first-n n (do-series series #'homes #'home-away-win-result #'home-away-odds)))
(defun do-series-away-wins-list (series &optional (n 30))
  (first-n n (do-series series #'aways #'home-away-win-result #'home-away-odds)))

(defun do-series-defeats-list (series &optional (n 30))
  (first-n n (do-series series #'home-aways #'home-away-lost-result #'home-away-lost-odds)))
(defun do-series-home-defeats-list (series &optional (n 30))
  (first-n n (do-series series #'homes #'home-away-lost-result #'home-away-lost-odds)))
(defun do-series-away-defeats-list (series &optional (n 30))
  (first-n n (do-series series #'aways #'home-away-lost-result #'home-away-lost-odds)))

(defun do-series-draws-list (series &optional (n 30))
  (first-n n (do-series series #'home-aways #'home-away-draw-result #'series-draw-odds)))
(defun do-series-home-draws-list (series &optional (n 30))
  (first-n n (do-series series #'homes #'home-away-draw-result #'series-draw-odds)))
(defun do-series-away-draws-list (series &optional (n 30))
  (first-n n (do-series series #'aways #'home-away-draw-result #'series-draw-odds)))

(defun do-series-overs-list (series &optional (n 30))
  (first-n n (do-series series #'home-aways #'series-overs #'series-over-odds)))
(defun do-series-home-overs-list (series &optional (n 30))
  (first-n n (do-series series #'homes #'series-overs #'series-over-odds)))
(defun do-series-away-overs-list (series &optional (n 30))
  (first-n n (do-series series #'aways #'series-overs #'series-over-odds)))

(defun do-series-unders-list (series &optional (n 30))
  (first-n n (do-series series #'home-aways #'series-unders #'series-under-odds)))
(defun do-series-home-unders-list (series &optional (n 30))
  (first-n n (do-series series #'homes #'series-unders #'series-under-odds)))
(defun do-series-away-unders-list (series &optional (n 30))
  (first-n n (do-series series #'aways #'series-unders #'series-under-odds)))

(defun series-table (my-list)
  (format-table t my-list
				:column-label '("League" "Team" "Stake" "Return" "Percents")
				:column-align '(:center :left :center :center :center)))

(defun do-series-wins (series &optional (n 10))
  (series-table (do-series-wins-list series n)))
(defun do-series-home-wins (series &optional (n 30))
  (series-table (do-series-home-wins-list series n)))
(defun do-series-away-wins (series &optional (n 30))
  (series-table (do-series-away-wins-list series n)))

(defun do-series-defeats (series &optional (n 30))
  (series-table (do-series-defeats-list series n)))
(defun do-series-home-defeats (series &optional (n 30))
  (series-table (do-series-home-defeats-list series n)))
(defun do-series-away-defeats (series &optional (n 30))
  (series-table (do-series-away-defeats-list series n)))

(defun do-series-draws (series &optional (n 30))
  (series-table (do-series-draws-list series n)))
(defun do-series-home-draws (series &optional (n 30))
  (series-table (do-series-home-draws-list series n)))
(defun do-series-away-draws (series &optional (n 30))
  (series-table (do-series-away-draws-list series n)))

(defun do-series-overs (series &optional (n 30))
  (series-table (do-series-overs-list series n)))
(defun do-series-home-overs (series &optional (n 30))
  (series-table (do-series-home-overs-list series n)))
(defun do-series-away-overs (series &optional (n 30))
  (series-table (do-series-away-overs-list series n)))

(defun do-series-unders (series &optional (n 30))
  (series-table (do-series-unders-list series n)))
(defun do-series-home-unders (series &optional (n 30))
  (series-table (do-series-home-unders-list series n)))
(defun do-series-away-unders (series &optional (n 30))
  (series-table (do-series-away-unders-list series n)))

(defparameter series-list `((,s1 "s1")
							(,s2 "s2")
							(,s3 "s3")
							(,s4 "s4")
							(,s5 "s5")))

(defparameter series-funcs
  (list #'do-series-wins-list
		#'do-series-home-wins-list
		#'do-series-away-wins-list
		#'do-series-draws-list
		#'do-series-home-draws-list
		#'do-series-away-draws-list
		#'do-series-defeats-list
		#'do-series-home-defeats-list
		#'do-series-away-defeats-list))

(defun write-series (series filename)
  (with-open-file (stream filename
						  :direction :output
						  :if-exists :supersede)
	(dolist (fn series-funcs)
	  (dolist (my-list (funcall fn series 20))
		(format stream "~a,~a,~a~%"
				(first my-list)
				(second my-list)
				(fifth my-list)))
	  (format stream "~%"))))

(defun export-series ()
  (dolist (series-pair series-list 'done)
	(destructuring-bind (series series-name) series-pair
	  (let ((filename (format nil "c:/mine/lisp/data/series ~a.csv" series-name)))
		(format t "~%Writing ~a..." filename)
		(write-series series filename)))))

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

	(format t "~{~{~%~a  ~a ~31tv ~a ~55t~a  ~a  ~5,2f ~5,2f ~5,2f  : ~6,2f ~6,2f~}~}" my-list)
	(format t "~%~%Stake  : £~,2f~%Return : £~,2f" stake returns)))

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

(defparameter series-results '("Wins" "Defeats" "Draws"))
(defparameter series-fns (list #'home-away-win-result
							   #'home-away-lost-result
							   #'home-away-draw-result))
(defparameter series-odds-fns (list #'home-away-odds
									#'home-away-lost-odds
									#'series-draw-odds))

(defun series-all-table (my-list)
  (format-table t my-list
				:column-label '("Result" "League" "Team" "Stake" "Return" "Percents")
				:column-align '(:center :left :center :center :center :center))
  t)

;;; Tried to get this to print percents as a formatted string "~6,2f",
;;; then having another numerical column to be used for sorting but not printed,
;;; but this causes an error with format-table unless all columns are listed.

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

(defun calc-team-series (team s games-fn result-fn odds-fn)
  "Adapted from do-team-series, calculates returns of a TEAM with series S,
   using games returned from GAMES-FN which match RESULT-FN at odds ODDS-FN"
  
  (let ((stake 0)
		(returns 0))
	(series-reset s)
	
	(dolist (game (funcall games-fn team))
	  (let ((current (series-current s)))
		(+= stake current)
		(cond ((funcall result-fn team game)
			   (+= returns (* current (funcall odds-fn team game)))
			   (series-update s "W"))
			  (t (series-update s "L")))))
	(values stake returns)))

(defun best-series-table (my-list)
  (format t "~%")
  (format-table t my-list
				:column-label '("Series" "Result" "Stake" "Return" "Percents")
				:column-align '(:center :center :center :center :center))
  t)

(defun best-series (team &optional (n 15))
  "Calculates best series for given TEAM for all series and results"
  (let ((my-list nil))
	(dolist (series-pair series-list)
	  (destructuring-bind (series series-name) series-pair

		(mapcar #'(lambda (result result-fn odds-fn)
					(multiple-value-bind (stake returns)
						(calc-team-series team series #'home-aways result-fn odds-fn)
					  (push (list series-name
								  result
								  stake
								  (my-round returns 0.01)
								  (calc-percent stake returns))
							my-list)))
				series-results series-fns series-odds-fns)))

	(best-series-table
	 (first-n n (sort my-list #'> :key #'fifth)))))

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
			  (list (string-upcase (car league))
					(reduce-list league)))
		  *db*))

(defun draws-per-game-week ())
