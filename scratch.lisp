;;;; Welcome to Portacle, the Portable Common Lisp Environment.
;; For information on Portacle and how to use it, please read
;;   https://portacle.github.io or *portacle-help*
;; To report problems and to get help with issues,  please visit
;;   https://github.com/portacle/portacle/issues
;; 
;; You can use this buffer for notes and tinkering with code.

(defun home (team game) (equal team (home-team game)))

(defun get-games (fn team)
  (remove-if-not
   #'(lambda (game)
	   (funcall fn team game))
   (get-league (find-league team))))

(defun homes (team)
  (get-games #'home team))

(defun homesx (team remove-list)
  (let ((my-list nil))
	(mapcar #'(lambda (game)
  )))

  (mapcar #'(lambda (game)
			  (remove-if
			   #'(lambda (xteam)
				   (equal xteam (away-team game))
				   ;(funcall #'away xteam game)
				   )
			   remove-list))
		  (homes team)))


mapcar - takes a function and one or more lists and returns the result
of applying the function to each element of the lists until a list runs out
(mapcar #'(lambda (x) (+ x 10)) '(1 2 3)) => (11 12 13)
(mapcar #'list '(a b c) '(1 2 3 4) => ((A 1) (B 2) (C 3)))

(defvar my-list '(2 3 4 5 6))
(mapcar #'square my-list) => (4 9 16 25 36)

maplist takes same args but calls the function on successive cdrs of the list
(maplist #'(lambda (x) x) '(A B C)) => ((A B C) (B C) (C))


 
