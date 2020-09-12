(defun day-of-week (day month year)
  (nth-value 6 (decode-universal-time (encode-universal-time 0 0 0 day month year 0))))

;; returns
;; 0-Fri 1-Sat 2-Sun 3-Mon 4-Tue 5-Wed 6-Thu
(defun football-day-of-week (day month year)
  (let ((d (+ 3 (day-of-week day month year))))
    (cond ((> d 6) (- d 7))
          (t d))))

(defvar *months*
  '((01 31)
    (02 28)
    (03 31)
    (04 30)
    (05 31)
    (06 30)
    (07 31)
    (08 31)
    (09 30)
    (10 31)
    (11 30)
    (12 31)))

(defun get-days-in-month (m)
  (second (assoc m *months*)))

(defun get-date (dt)
  (mod dt 100))

(defun get-month (dt)
  (truncate (/ (mod dt 10000) 100)))

(defun get-year (dt)
  (truncate (/ dt 10000)))

(defun add-to-date (dt days)
  (let ((date (get-date dt))
        (month (get-month dt))
        (year (get-year dt)))
    (+= date days)
    (if (> date (get-days-in-month month))
        (progn
          (-= date (get-days-in-month month))
          (incf month)
          (if (> month 12)
              (progn
                (-= month 12)
                (incf year)))))
    (+ (* year 10000)
       (* month 100)
       date)))

#|
(defun do-season (csv-league)
  (let* ((league (get-league csv-league))
         (start-date (integer-date (caar league)))
         (week-list nil)
         (season-list nil))

    (labels ((inner (list)
               (cond ((null list)
                      (push (reverse week-list) season-list)
                      (reverse season-list))
                     ((<= (integer-date (date (car list))) (+ start-date 5)) ; fits within this week
                      (push (car list) week-list)
                      (inner (rest list)))
                     (t  ; new week
                      (push (reverse week-list) season-list)
                     (cond ((null (cdr list)) ; only one game for this week at the very end of the list
                             (push (car list) week-list)
                             (inner (rest list))) ; goto null list above and exit
                            (t
                             (setf start-date (integer-date (date (cadr list))))
                             (setf week-list nil)
                             (inner list))))))) ; havent  done anything with this line yet
      (inner league))))

(defun test ()
  (mapcar #'(lambda (x)
              (print x)
              (read-line))
          (do-season "e0")))

(defun test2 ()
  (dolist (league *leagues*)
    (mapcar #'(lambda (x)
                (print x))            
          (do-season (car league)))))




(defun percents (league)
  (mapcar
   #'(lambda (x)
       `(,x ,(home-away-percentage-return x))) 
   (get-teams league)))


(defun max-percents (league)
  (format t "~{~%~a~}"
          (msafe-sort (percents league)
            #'> :key #'second)))
|#

