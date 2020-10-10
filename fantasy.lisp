
;; fantasy.lisp 20/06/20

(defpackage :fantasy
  (:use :cl :cl-json :parse-float :trivial-download))

(defvar *fantasy-db*)
(defvar *teams*)
(defvar *positions* '(Goalkeeper Defender Midfield Forward))

(defun get-players ()
  (cdr (assoc :elements *fantasy-db*)))

(defun edit-teams (data)
  (mapcar #'(lambda (player)
			  (cond ((equal (team player) 9)
					 (setf (cdr (assoc :team player)) (list 10)))
					((equal (team player) 10)
					 (setf (cdr (assoc :team player)) (list 9)))))
		  data)
  data)

(defparameter *players* (edit-teams (get-players)))

(defun get-first ()
  (car (get-players)))

(defun first-name (player)
  (cdr (assoc :first--name player)))

(defun surname (player)
  (cdr (assoc :second--name player)))

(defun name (player)
  (format nil "~a ~a" (first-name player) (surname player)))

(defun team (player)
  (nth (1- (cdr (assoc :team player))) *teams*))

(defun total-points (player)
  (cdr (assoc :total--points player)))

(defun points-per-game (player)
  (parse-float:parse-float (cdr (assoc :points--per--game player))))

(defun field-position (player)
  (nth (1- (cdr (assoc :element--type player))) *positions*))

(defun price (player)
  (/ (cdr (assoc :now--cost player)) 10))

(defun read-json (filename)
  (with-open-file (stream filename)
	(json:decode-json stream)))


(defun fantasy ()
  (setf *fantasy-db* (read-json "c:/mine/lisp/data/fantasy.json"))
  (setf *teams* (cdr (assoc :|*PREMIER *LEAGUE|
							(read-json "c:/mine/perl/football/data/teams.json"))))
  (edit-teams (get-players))
  t)

(defun update ()
  (trivial-download:download
   "https://fantasy.premierleague.com/api/bootstrap-static/"
   "c:/mine/lisp/data/fantasy.json")
  (fantasy))


(defun get-keepers (&key (price 5))
  (get-by-position "Goalkeeper" price))
(defun get-defenders (&key (price 5))
  (get-by-position "Defender" price))
(defun get-midfielders (&key (price 5))
  (get-by-position "Midfield" price))
(defun get-forwards (&key (price 5))
  (get-by-position "Forward" price))

(defun show-info (player)
  (format t "~% £~4,1fm - ~a (~a) ~62t ~3d  ~4,1f"
		  (price player) (name player) (team player)
		  (total-points player) (points-per-game player)))

(defun show-player (player)
  (format t "~% ~a ~a : ~a ~a ~a => £~,1fm"
		  (name player) (team player) (field-position player)
		  (total-points player) (points-per-game player) (price player)))

(defun do-players ()
  (dolist (player (get-players))
	(show-player player)))

(defun get-team (team)
  (remove-if-not #'(lambda (player)
					 (string-equal team (team player)))
				 *players*))

(defun get-team-players (team)
  (mapcar #'(lambda (player)
			  (show-player player))
		  (get-team team))
  t)

(defun get-by-position (position price)
  (remove-if-not
   #'(lambda (player)
	   (and (string-equal (field-position player) position)
			(>= (price player) price)))
   (get-players)))

(defun show-all (list &key key)
  (mapcar #'(lambda (player)
			  (show-info player))
		  (sort list #'> :key key))
  t)

(defun show-by-price (list)
  (show-all list :key #'price))

(defun show-by-points (list)
  (show-all list :key #'total-points))

(defun show-by-points-per-game (list)
  (show-all list :key #'points-per-game))

(defun get-team (team)
  (remove-if-not
   #'(lambda (player)
	   (string-equal (team player) team))
   (get-players)))

;;do really need lower bracket ?? - just under/upper
(defun price-range (price lower upper)
  (and (>= price lower)
	   (<= price upper)))

#|
get players in range position lower upper
lambda player
price-range lower upper
getbypositiom ??
|#
