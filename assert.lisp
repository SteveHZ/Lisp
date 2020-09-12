(defpackage :zappa-assert
  (:use :common-lisp))

;;  Adapted from statistics.lisp

(defmacro my-assert (&rest args)
  (let ((assertions nil))
    (dolist (arg args (append `(or ,@(nreverse assertions))))
      (let* ((name (first arg))
             (type (second arg))
             (test (case type
                     ((:positive-integer :posint)
                      `(and (integerp ,name) (plusp ,name)))
                     ((:positive-number :posnum)
                      `(and (numberp ,name) (plusp ,name)))
                     ((:non-negative-integer :non-negint)
                      `(and (integerp ,name) (>= ,name 0)))
                     ((:non-negative-number :non-negnum)
                      `(and (numberp ,name) (>= ,name 0)))
                     (t `(typep ,name ',type))))
             (message `(error
                        ,(if (eql type :test)
                             name
                             (format nil "~a = ~~a is not a ~a" name
                                     (case type
                                       ((:positive-integer :posint)
                                        "positive integer")
                                       ((:positive-number :posnum)
                                        "positive number")
                                       ((:non-negative-integer :non-negint)
                                        "non-negative integer")
                                       ((:non-negative-number :non-negnum)
                                        "non-negative number")
                                       (t type))))
                        ,@(unless (eql type :test) `(,name)))))
        (push `(unless ,test ,message) assertions)))))

