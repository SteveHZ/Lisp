; Chapter 3
(defun my-double (x)
  (* x 2))
(defun my-square (x)
  (* x x))
(defun half (x)
  (/ x 2))
(defun cube (x)
  (expt x 3)) ;  (* x x x))
(defun onemorep (x y)
  (equal x(+ y 1)))
(defun pythag (x y)
  (sqrt (+ (my-square x) (my-square y))))
(defun miles-per-gallon (initial-speedo final-speedo gallons-used)
  (/ (- final-speedo initial-speedo) gallons-used))
(defun hello ()
  (write-line "What is your name ?")
  (let ((name (read-line)))
  (format t "Hello, ~A.~%" name)))

; (setf name (prompt "Please enter name :"))
;(defun prompt (str &optional (prmpt ">"))
(defun prompt (str)
  (format t "~%~a " str)
	(read-line))
(defun longer-than (x y)
  (> (length x) (length y)))
(defun add-length (x)
  (cons (length x) x))

(defun f-to-c (fahr) ; [5×(Fahrenheit  temperature  -  32)]/9
  (* (- fahr 32) 5/9))
(defun c-to-f (cels) ; (9/5×Celsius temperature) + 32
  (+ 32 (* cels 9/5)))
(defun add-mid1 (x)
  (list (first x)
        (+ 1 (second x))
        (third x)))
(defun my-fun (x y)
  (list (list x) y))
(defun my-equalp (x y)
  (equal x (first y)))

; Chapter 4
(defun make-even (x)
  (if (evenp x) x (+ 1 x)))
(defun further (x)
  (if (> x 0)
    (+ x 1)
    (- x 1)))
(defun my-not (x)
  (if x nil t))
(defun ordered (x y)
  (if (< x y) (list x y)
              (list y x)))
(defun my-abs (x)
  (cond ((< x 0) (- x))
        (t x))) ; similar to switch default
(defun make-odd (x)
  (cond ((not(oddp x)) (+ x 1))
        (t x)))
(defun geqp (x y) ;(>= x y))
  (or (> x y)
      (equal x y)))
(defun wtf (x)
  (cond ((and (oddp x) (> x 0)) (* x x))
        ((and (oddp x) (< x 0)) (* x 2))
        (t (/ x 2))))
; SIMPLE HASHES
; Usage (my-make-hash ht '((one un)(two deux)(three trois)))
; DONT USE THIS ! create with (defvar ht '((...))') instead
(defun my-make-hash (ht data)
  (setf ht data))
; Usage (get-ht-val ht 'two)
(defun get-hash-val (ht key)
  (rest (assoc key ht)))

(defun get-result (home away)
  (cond ((> home away) 'H)
        ((< home away) 'A)
        (t 'D)))

(defun do-squares (x)
  (mapcar #'my-square x))

(defun all-odd (lst)
  (every #'oddp lst))

(defun none-odd (lst)
  (every #'evenp lst))

(defun find-even (lst)
  (find-if #'evenp lst))

(defun find-odd (lst)
  (find-if #'oddp lst))

(defun read-file (var file)
  (with-open-file (stream file)
    (with-standard-io-syntax
      (setf var (read stream)))))

(defun compare1 (x y)
  (cond ((> x y) "first is bigger")
        ((< x y) "first is smaller")
        (t "numbers are the same")))

(defun compare2 (x y)
  (if (< x y) "first is smaller"
    (if (> x y) "first is bigger"
      "numbers are the same")))

(defun compare3 (x y)
  (or (and (< x y) "first is smaller")
      (and (> x y) "first is bigger")
      "numbers are the same"))

;; compare to Perl -
;; return "blah" if $x==$y etc

(defun gtest (x y)
  (or (> x y)
      (zerop x)
      (zerop y)))

(defun gtest-cond (x y)
  (cond ((> x y) t)
        ((zerop x) t)
        ((zerop y) t)
        (t NIL)))

(defun boilingp (scale temp)
  (cond ((and (equal scale 'f)
              (> temp 212)))
        ((and (equal scale 'c)
              (> temp 100)))
        (t NIL)))

(defun boilingp2 (scale temp)
  (or (and (equal scale 'f)
           (> temp 212))
      (and (equal scale 'c)
           (> temp 100))))

;; ch 8 recursion

(defun anyoddp (x)
  (cond ((null x) nil)              ; end recursion NIL
        ((oddp (first x)) t)        ; return x and exit from recursion
        (t (anyoddp (rest x)))))    ; recurse into rest of list

(defun anyoddp-or (x) ;; prefer this idiom to above
  (if (null x) nil
      (or (oddp (first x))
          (anyoddp-or (rest x)))))

(defun fact (x)
  (cond ((zerop x) 1)
        (t (* x (fact (- x 1))))))

(defun fact2 (x) ; and again
  (if (zerop x) 1
      (* x (fact (- x 1)))))

(defun laugh (n)
  (cond ((zerop n) nil)
        (t (cons 'ha (laugh (- n 1))))))

(defun laugh2 (n)
  (if (zerop n) nil
      (cons 'ha (laugh2 (- n 1)))))

(defun add-up (x)
  (cond ((null x) 0)
        (t (+ (first x) (add-up (rest x))))))

(defun add-up2 (x)
  (if (null x) 0
      (+ (first x) (add-up2 (rest x)))))

;; on lisp p68
(defun my-every (fn lst)
  (if (null lst) t                   ; reached the end, everything has been true so far
      (and (funcall fn (first lst))  ; may fail at some point and short-circuit returning NIL without going to next line
           (my-every fn (rest lst)))))

;; basic lisp techniques p42
(defun safe-sortx (list predicate)
  (let ((new-list (copy-list list)))
    (sort new-list predicate)))

(defun rec-member (x list)
  (if (null list) nil)
  (or (equal x (first list))
      (rec-member x (rest list))))

(defun fib (n)
  (cond ((equal n 0) 1)
        ((equal n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

(defun last-element (x)
  (if (atom (cdr x)) (car x)
      (last-element (rest x))))

(defun add-nums (n)
  (if (zerop n) 0
      (+ n (add-nums (- n 1)))))

(defun all-equal (x)
  (cond ((null (rest x)) t)
        ((not (equal (first x) (second x))) nil)
        (t (all-equal (rest x)))))

(defun count-down (n)
  (if (zerop n) nil
      (cons n (count-down (- n 1)))))
(defun count-down-cond (n)
  (cond ((zerop n) nil)
        (t (cons n (count-down (- n 1))))))
(defun applic-fact (n)
  (apply #'* (count-down n)))
(defun square-list (x)
  (if (null x) nil
      (cons (* (first x)(first x))
            (square-list (rest x)))))

(defun my-nth (n x)
  (cond ((null x) nil)
        ((zerop n) (first x))
        (t (my-nth (- n 1) (rest x)))))

(defun compare-lengths (x y)
  (cond ((and (null x)
			  (null y)) 'same-length)
        ((null y) 'first-is-longer)
        ((null x) 'second-is-longer)
        (t (compare-lengths (rest x)
                            (rest y)))))

(defun count-up (n)
  (labels ((count-up-recursive (cnt)
             (if (> cnt n) nil
                 (cons cnt
                       (count-up-recursive
                        (+ cnt 1))))))
    (count-up-recursive 1)))
    
(defun x (y)
  (if (= (y 0)
    )))