
;; Save here, then to cl-repl.org

;; Helper functions

(defun rpo-from-deliveries (score delivs)
  (* 6 (/ score delivs)))

;; (split-deliveries 5.3) returns (values 5 3)

(defun split-deliveries (overs)
  (multiple-value-bind (ovs delivs) (floor overs)
	(values ovs (round (* 10 delivs)))))

;; (get-deliveries 5.3) returns 33

(defun get-deliveries (overs)
  (multiple-value-bind (ovs delivs) (split-deliveries overs)
	(+ (* 6 ovs)
	   delivs)))

(defun runs-per-ball (runs-per-over)
  (/ runs-per-over 6))

(defun calc-runline (score overs runline-overs)
  (* (get-deliveries runline-overs)
	 (runs-per-ball (rpo-from-deliveries score (get-deliveries overs)))))

;; Find runs per over from either full or part overs (eg 5.3 overs)

(defun rpo (score overs)
  (let ((delivs (get-deliveries overs)))
	(format t "~,2f" (rpo-from-deliveries score delivs))))

;; Find predicted runline amount

(defun runline (score overs runline-overs)
  (format t "~,2f" (calc-runline score overs runline-overs)))

;; Find the equivalent runs-per-6 ball over score for a hundred score

(defun hundred (score &optional (delivs 100))
  (format t "~,2f" (rpo-from-deliveries score delivs)))


;; Odds conversions

(defun decimal-to-percent (odds)
  (* 100 (/ 1.0 odds)))

(defun percent-to-decimal (pc)
  (/ 100.0 pc))

(defun frac-to-decimal (str-frac)
  (let ((nums (ppcre:split "-" str-frac)))
	(+ 1.0 (/ (parse-integer (first nums))
			  (parse-integer (second nums))))))

(defun frac-to-percent (frac)
  (decimal-to-percent
   (frac-to-decimal frac)))

;; Stake calculator

(defun calc-new-stake (series-amount odds)
  (ceiling
   (* series-amount
	  (/ 1 (1- odds)))))

(defun calc-stake (series-amount odds)
  (let* ((new-stake (calc-new-stake series-amount odds))
		 (new-return (* new-stake odds)))
	(format t "~%New Stake  : £~6,2f" new-stake)
	(format t "~%Return     : £~6,2f" new-return)
	(format t "~%Profit     : £~6,2f" (- new-return new-stake))))

