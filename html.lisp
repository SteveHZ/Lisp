
;; html.lisp 24/01/21

(defun start-server (port &key (doc-root #p"html/"))
  (make-instance 'hunchentoot:easy-acceptor
				 :port port
				 :document-root doc-root))

(defmacro as (tag content)
  `(format t "<~(~a~)>~a</~(~a~)>"
		   ',tag ,content ',tag))

(defmacro with (tag &rest body)
  `(progn
	 (format t "~&<~(~a~)>~%" ',tag)
	 ,@body
	 (format t "~&</~(~a~)>~%" ',tag)))

(defun brs (&optional (n 1))
  (fresh-line)
  (dotimes (i n)
	(princ "<br>"))
  (terpri))

(defun css (filename)
  (format t "~&<link rel='stylesheet' type='text/css' href='css/~a.css'/>" filename))

(defun html-file (base)
  (format nil "html/~(~a~).html" base))

(defmacro head (&rest body)
  `(progn
	 (format t "~&<head>~%")
	 ,@body
	 (format t "~&</head>")))

(defmacro body (&rest body)
  `(progn
	 (format t "~&<body>~%")
	 ,@body
	 (format t "~&</body>")))

(defmacro page (name title &rest body)
  (let ((ti (gensym)))
	`(with-open-file (*standard-output*
					  (html-file ,name)
					  :direction :output
					  :if-exists :supersede)
	   (let ((,ti ,title))
		 (head
		  (as title ,ti)
		  (css "MySpeedDial"))
		 (body
		  (with center
				(as h2 (string-upcase ,ti)))
		  (brs 3)
		  ,@body)))))

(defmacro with-link (dest &rest body)
  `(progn
	 (format t "<a href-\"~a\">" (html-file ,dest))
	 ,@body
	 (princ "</a>")))

(defun link-item (dest text)
  (princ "<li>")
  (with-link dest
	(princ text)))

(defun button (dest text)
  (princ "[")
  (with-link dest
	(princ text))
  (format t " ]~%"))
