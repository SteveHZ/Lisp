(defvar cards)
(setf cards '(2 3 4 5 6 7 8 9 10 jack queen king))

(defvar my-hand)
(setf my-hand '((3 clubs)
                (5 spades)
                (7 diamonds)
                (4 diamonds)
                (ace spades)))

(defvar colours)
(setf colours '((clubs black)
                (diamonds red)
                (hearts red)
                (spades black)))

(defun rank (card) (first card))
(defun suit (card) (second card))

(defun colour-of (card)
  (second (assoc (suit card) colours)))

(defun count-suit (st hand)
  (length (remove-if-not
           #'(lambda (card) (equal (suit card) st))
           hand)))

(defun higher-rank-p (x y lst)
   (beforep x y lst))

(defun beforep (x y lst)
   (member x (member y lst)))

; USAGE :
; (grep (lambda (x) (< x 5) '(1 2 3 4 5 6 7 8 9)))
; (grep #'oddp '(1 2 3 4 5 6 7 8 9))

(defmacro my-grep (fn lst)
 `(remove-if-not ,fn ,lst))

(defun count-suit2 (st hand)
                   (equal (suit card) st)) hand)))

; USAGE :
; (my-map (lambda(x) (* x 3)) lst)
(defmacro my-map (fn lst)
  `(mapcar ,fn ,lst))

(defun first-red (hand)
  (find-if #'(lambda (x) (equal (colour-of x) 'red)) hand))

(defun black-cards (hand)
  (remove-if-not #'(lambda (x) (equal (colour-of x) 'black)) hand))

(defun black-cards2 (hand)
  (my-grep (lambda (x) (equal (colour-of x) 'black)) hand))

(defun what-ranks (s hand)
  (mapcar #'first (remove-if-not #'(lambda (x) (equal (suit x) s)) hand)))
