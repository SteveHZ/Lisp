(ql:quickload "read-csv")
(use-package :read-csv)

(defvar *db* nil)
(setf filename "E0.csv")

(defun my-read-csv (filename)
  (with-open-file (*db* filename)
    (parse-csv *db*)))

;;(defun my-read-csv (filename)
;;  (with-open-file (s filename)
;;    (parse-csv s)))
;; Returns a list of lists of strings.

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))
