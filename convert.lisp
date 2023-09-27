
;; Centigrade to Fahrenheit

(defun c2f-calc (c)
  (+ 32
	 (* c (/ 9 5))))

(defun c2f (c)
  (format t "~,2f deg C = ~,2f deg F"
		  c (c2f-calc c)))

;; Fahrenheit to Centigrade

(defun f2c-calc (f)
  (* (- f 32)
	 (/ 5 9)))

(defun f2c (f)
  (format t "~,2f deg F = ~,2f deg C"
		  f (f2c-calc f)))

;; Miles to Kilometres

(defun m2km (m)
  (format t "~,2f miles = ~,2f km" m (/ m 0.62137)))

;; Kilometers to Miles

(defun km2m (km)
  (format t "~,2f km = ~,2f miles" km (* km 0.62137)))

;; Centimetres to Inches

(defun cm2in (cm)
  (format t "~,2f cm = ~,2f inches" cm (* cm 0.39)))

;; Inches to Centimetres

(defun in2cm (in)
  (format t "~,2f in = ~,2f cm" in (* in 2.54)))
