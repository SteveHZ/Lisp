(defpackage :zappa-macros
  (:use :common-lisp))

(defmacro input (var string)
  `(format t "~%")
  `(setf ,var (prompt ,string)))

;; PCL version
(defmacro pcl-with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;; Ansi Common Lisp version
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
          syms)
     ,@body))

(defmacro my-for (var (start stop &key (step 1)) &body body)
  (with-gensyms (gstop gstep)
    `(do ((,var ,start (+ ,var ,gstep))
          (,gstop ,stop)
          (,gstep ,step))
         ((> ,var ,gstop)
          (- ,var ,gstep))
       ,@body)))

(defmacro my-forx (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro my-for-step (var start stop step &body body)
  (with-gensyms (gstop gstep)
    `(do ((,var ,start (+ ,var ,gstep))
          (,gstop ,stop)
          (,gstep ,step))
         ((> ,var ,gstop)
          (- ,var ,gstep))
        ,@body)))

(defmacro my-while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro my-when (condition &body body)
  `(if ,condition (progn ,@body)))

(defmacro my-unless (condition &body body)
  `(when (not ,condition)
	 ,@body))

(defmacro my-unlessx (test &body body)
  `(do ()
	   (,test)
	 ,@body))

(defmacro new (obj var &rest body)
  `(setf ,var (make-instance ,obj ,@body)))

(defmacro += (var amount)
  `(setf ,var (+ ,var ,amount)))

(defmacro -= (var amount)
  `(setf ,var (- ,var ,amount)))

(defmacro *= (var amount)
  `(setf ,var (* ,var ,amount)))

(defmacro //= (var amount)
  `(setf ,var (float (/ ,var ,amount))))

;	example call (mac (for x 1 10))
(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro ne (a b)
  `(not (equal ,a ,b)))

(defmacro string-ne (a b)
  `(not (string-equal ,a ,b)))

(defun read-filex (filename)
  (let ((in (open filename :if-does-not-exist nil))
        (mylist (list)))
    (when in
      (loop for line = (read-line in nil)
            while line do (push line mylist))
      (close in))
    (reverse mylist)))

(defun iterate-list (mylist)
  (dolist (item mylist)
    (format t "~a~%" item)))

(defun foreach (mylist fn)
  (dolist (item mylist)
    (funcall fn item)))

;; example call
;; (defun test (my (x 4) (my (x 5) (print (+ x y)))))
(defmacro my ((var val) &body body)
  `(let ((,var ,val))
     ,@body))

;; call as (sprintf "~a :~a" 'steve 'zappa)
(defmacro sprintf (str &rest args)
  (let ((gstr (gensym)))
    `(let ((,gstr ,str))
	   (format nil ,gstr ,@args))))

;; example call
;; (safe-sort *expects* #'> :key #'game-goal-diff)
(defmacro safe-sort (list &body body)
  "Macro to safely sort LIST, accepts keyword arguments in BODY"
  `(sort (copy-list ,list)
         ,@body))

(defmacro defprint-class (instance-name class-name &body body)
  "Macro to create PRINT-OBJECT function for any CLASS-NAME"
  `(defmethod print-object ((,instance-name ,class-name) stream)
     (print-unreadable-object (,instance-name stream :type t))
     ,@body))

(defmacro unless-zerop (arg &body body)
 "Return 0 if argument ARG is zero to avoid division-by-zero errors, otherwise continue"
  `(if (= ,arg 0) 0
	   ,@body))

;; Ternary operator in lisp eg my $x = ($y > 10) ? y * 2 : y * 10

(defmacro let? (var test expr1 expr2 &body body)
  `(let ((,var
		   (if ,test ,expr1 ,expr2)))
	 ,@body))

;;(defmacro xor (flag)  `(logxor ,flag 1))

