(*** Exercice  1  ***)
 
(defparameter l1 '(1 2 3))
(defparameter l2 '(4 5 6))
(defparameter l (nconc l1 l2))
l ; --> (1 2 3 4 5 6)
(print l)

(defun my-nconc (l1 l2)
  "My NCONC"
  (setf (cdr (last l1)) l2)
  )
(my-nconc l1 l2)
 
(*** Exercice  2  ***)
(setf (cdr (last l1)) 'l1)
(print l1)

(*** Exercice  3  ***)
(let ((u-courant 1))
  (defun gen-p ()
    (labels ((suivant (x) (+ (* 3 x) 2)))
      (setf u-courant (suivant u-courant)))
    )
  )
(gen-p)

(*** Exercice  4  ***)
 
(defmacro first_ (l)
  `(car ,l))

(macroexpand '(first_ (f (g x y))))
(macroexpand-1 '(first_ (f (g x y))))
 
(defmacro second_ (l)
  `(first_ (cdr ,l)))

(macroexpand-1 '(second_ x))
(macroexpand '(second_ x))
 
(defmacro third_ (l)
  `(caddr ,l))
 
(*** Exercice  5  ***)
 
(*** Exercice  6  ***)
 
(defun exemple-mycond (n)
    "un exemple d'utilisation de my-cond"
    (my-cond 
     ((zerop n) 'zero)        
     ((= 1 n) 'un)
     ((= 2 n) 'deux)))

(exemple-mycond 1) ; --> UN
(exemple-mycond 5) ; --> ERROR
 
(*** Exercice  7  ***)
 
(macroexpand '(my-let 
                 ((x 45) (y (+ 3 4))) 
                 (print x) 
                 (cons x y)))
; --> (FUNCALL (LAMBDA (X Y) (PRINT X) (CONS X Y)) 45 (+ 3 4))
 
(*** Exercice  8  ***)
 
(macroexpand-1 '(mcons 1 2 3 4 5))
(macroexpand '(mcons 1 2 3 4 5)) 
(mcons 1 2 3 4 5)

(macroexpand-1 '(mmcons 1 2 3 4 5))
(macroexpand '(mmcons 1 2 3 4 5)) 
(mmcons 1 2 3 4 5)
 
(*** Exercice  9  ***)