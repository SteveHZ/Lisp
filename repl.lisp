(defparameter *allowed-commands* '(homes aways quit))

(defun game-repl (&optional (prompt "CL-Zappa > "))
  (let ((cmd (game-read prompt)))
    (unless (eq (first cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl prompt))))

(defun game-read (prompt)
  (princ prompt)
;(read-line)
  (read-from-string   (concatenate 'string "(" (read-line) ")")   )
  #|
  (read-from-string
   (concatenate 'string "(say (" (read-line) "))"))
  (let ((cmd (read-from-string
              (concatenate 'string "(say (" (read-line) "))"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd)))))
|#
  )

(defun game-eval (sexp)
  (print sexp)
  (if (member (car sexp) *allowed-commands*)
      (progn
;        (print "Got to Here")
        (let ((str (get-str sexp)))
;          (print "Second here")
          (print str)
          (eval str))
        )
      '(Unknown Command))
)


(defun get-str (s)
;  (read-from-string (concatenate 'string "(say " (car s) " \"" (cdr s) "\")"))
  (format nil "(say \"~@a\")" s)
;  (format nil "(say ~a\"~a\")" (car s) (cdr s))
 )



#|(let* ((s1 (cdr sexp))
              (str (format nil "(say ~a ~a)" (car sexp) s1)))
|#
#|        (let ((str (format nil "(say ~a)" sexp)))
)
|#
#|
(let ((str (read-from-string (concatenate 'string "(say " (car sexp) " \"" (cdr sexp) "\")")))) ; ;
  )
|#

#|(let ((str (format nil "~((say ~s)~)" sexp)))
          (eval str)))
          '(Unknown Command)))
|#
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eq item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))
