;(defmacro deftest (name parameters &body body)
;  `(defun ,name ,parameters    (let ((*test-name* ',name))
;      ,@body)))

;(defmacro += (var amount)
;	`(setf ,var (+ ,var ,amount)))

;(defmacro op-equals (op &rest body)
(defmacro opeq (name op)
  `(defun ,name (&rest params)
      (let ((my-var (first params)))
            (amount (second params))
            `(setf ,my-var (,op ,my-var ,amount)))))

;  `(defun ,name ,(&rest params)
;        (let ((var (first parameters)))
;        (let ((amount (second parameters)))
;(setf my-var (,op ,my-var,amount))))
;''        `(setf var '(#'op ,var,amount))))
;        `(setf var (#'op ,@body))))

(opeq *= *)
(opeq -= -)
