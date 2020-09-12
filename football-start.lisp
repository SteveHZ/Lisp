(in-package "COMMON-LISP-USER")

(let ((my-files
  '("c:/mine/lisp/my-poisson.lisp"
    "c:/mine/lisp/my-odds.lisp"
    "c:/mine/lisp/my-skellam.lisp"
	"c:/mine/lisp/my-expectdb.lisp"
    "c:/mine/lisp/football.lisp")))

  (princ "Loading files ")
  (dolist (filename my-files)
    (princ ".")
    (load filename)))
