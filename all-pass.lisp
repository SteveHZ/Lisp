(defun gt-than (x y)
  (> x y))

(defun gt10 (x) (gt-than x 10))
(defun gt20 (x) (gt-than x 20))
(defun gt30 (x) (gt-than x 30))
(defun gt40 (x) (gt-than x 40))
(defun gt50 (x) (gt-than x 50))

(defmacro do-all-pass (var amount)
  `(reduce (lambda (x y)
             (and x y))
           (funcall ,var ,amount)))

(defun all-pass (funcs)
  #'(lambda (x)
      (mapcar #'(lambda (fn)
                  (funcall fn x))
              funcs)))

;; Use all-pass to bind tests to a variable
;;   (defvar test (all-pass '(gt10 gt20 gt30)))

;; Use do-all-pass to run the tests
;;   (do-all-pass test 35) => T
;;   (do-all-pass test 15) => NIL

;; all-pass returns a list of true/false values eg (T T NIL) so calling
;;   (funcall test 15)
;;
;; doesn't work correctly, hence the do-all-pass macro which will
;; reduce the returned value to either T or NIL

;; all-pass returns just the lambda function, calling
;; (funcall ,var ,amount) in the macro provides the lambda value to all-pass
;; which then calls each provided function in turn


#| USAGE

CL-USER> (load "all-pass.lisp")
T
CL-USER> (setf test1 (all-pass '(gt10 gt20 gt30 gt40 gt50)))
#<CLOSURE (LAMBDA (X) :IN ALL-PASS) {1007334DBB}>

CL-USER> (inspect test1)

The object is a CLOSURE named (LAMBDA (X) :IN ALL-PASS).
0. Lambda-list: (X)
1. Ftype: (FUNCTION (T) (VALUES LIST &OPTIONAL))
2. Closed over values: ((GT10 GT20 GT30 GT40 GT50))

> (do-all-pass test1 65)
T
> (do-all-pass test1 45)
NIL

> (defvar test2 (all-pass '(numberp oddp)))

TEST2
> (do-all-pass test2 17)
T
> (do-all-pass test2 176)

NIL
> 
|#
