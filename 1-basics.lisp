(defun square(x)
  "SQUARE of x"
  (* x x))
(square 2)

(defun mean(x y)
  "MEAN of x and y"
  (/ (+ x y) 2))
(mean 2 4)

(defun eval-quadratic (a b c x)
  "EVAL-QUADRATIC given by a, b, c in x"
  (+ (* a x x) (* b x ) c))
(eval-quadratic 1 1 1 1)

(defun a ()
  (sqrt (+ 1 (sqrt (+ 2 (sqrt 3)))))
  )
(a)

(defun b ()
  (* (+ 2 3) (+ 4 5 6))
  )

(defun c ()
  (log (+ (* 99 99) 3))
  )
(c)

(defun d ()
  (/ (+ (a) (b)) (- (a) (b)))
  )
(d)

(defun e ()
  (- (/ (+ (a)
	   (b))
	(+ (a)
	   (* 2 (b))
	   )
	)
     (sqrt
      (/ (+ (a)
	    (* 2 (b)))
	 (+ (a) (b))
	 ))
     )
  )
(e)

(defun discriminant (a b c)
  "DISCRIMINANT calculus"
  (sqrt (- (* b b) (* 4 (* a c))))
  )
(discriminant 2 1 0)

(defun racine1 (a b c)
  "Return first RACINE"
  (/ (- (+ b (sqrt (discriminant a b c))))
     (* 2 a)
     )
  )
(racine1 1 -2 1)


(defun racine2 (a b c)
  "Return second RACINE"
  (/ (- (- b (sqrt (discriminant a b c))))
     (* 2 a)
     )
  )
(racine2 1 -2 1)

(defun carac-racines-trinome (a b c)
  "Display RACINE type"
  (if (> (discriminant a b c) 0)
      "2 real racines"
      (if (< (discriminant a b c) 0)
	  "2 complex racines"
	  "1 real double racine"
	  )
  ))
(carac-racines-trinome 1 -2 1)

(defun carac-racines-trinome-cond (a b c)
  "Cond version + local var"
  (let ((d (discriminant a b c)))
    (cond
      ((> d 0)
       "2 real racines")
      ((< d 0)
       "2 complex racines")
      (t
       "1 real double racine")
      )
    )
  )
(carac-racines-trinome-cond 1 -2 1)
