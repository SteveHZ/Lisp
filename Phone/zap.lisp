(defun rpo (score &optional (overs 20))
  (format t "~,2f" (/ score overs)))

; Find the equivalent runs-per-6 ball over
; score for a hundred score

(defun hundred (score &optional (balls 100))
  (format t "~,2f" (* 6 (/ score balls))))
  
; Odds conversions
                 
(defun decimal-to-percent (odds )
  (* 100 (/ 1.0 odds )))

(defun percent-to-decimal (pc )
  (/ 100.0 pc ))

(defun frac-to-decimal (str-frac )
  (let ((nums (ppcre:split "-" str-frac )))
    (+ 1.0 (/ (parse-integer (first nums ))
              (parse-integer (second nums ))))))
  
(defun frac-to-percent (frac )
  (decimal-to-percent
   (frac-to-decimal frac )))
   
; Calculate new stake
                    
(defun calc-new-stake (series-amount odds )
  (ceiling
         (* series-amount
            (/ 1 (1- odds )))))

(defun calc-stake (series-amount odds )
  (let* ((new-stake (calc-new-stake series-amount odds ))
         (new-return (* new-stake odds )))
    (format t "~%New Stake  : £ ~6,2f" new-stake )
    (format t "~%Return     : £ ~6,2f" new-return )
    (format t "~%Profit     : £ ~6,2f" (- new-return new-stake ))))
                  
