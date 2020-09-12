;; DO NOT USE OR EDIT THIS FILE !!!
;; EDIT C:/users/steve/portacle/start.lisp INSTEAD !!!

(swank:set-default-directory "c:/mine/lisp")
(ql:quickload "cl-csv")
;;(ql:quickload "cl-json")
;;(ql:quickload "iterate")

(load "macros.lisp")
(load "math.lisp")

(asdf:oos 'asdf:load-op :iterate)
(use-package :iterate)


;(load "c:/mine/lisp/quicklisp.lisp")
;(load "C:/Users/steve/quicklisp/setup.lisp")
;(load "C:/Mine/lisp/csv.lisp")
;(runcsv)
