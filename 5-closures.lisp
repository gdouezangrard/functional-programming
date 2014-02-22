(*** Exercice  1  ***)
(defun evaluate (f)
	"Apply function"
	(funcall f 1)
	)
(evaluate (lambda (x) (+ (* 3 (* x x)) 4.7)))
 
(*** Exercice  2  ***)
(defun sigma (f n p &optional (s 0))
	"Sum"
	(cond
		((= n p) (+ s (funcall f p)))
		(t (sigma f (1+ n) p (+ s (funcall f n))))
		)
	)
(sigma (lambda(x) x) 1 5)

(*** Exercice  3  ***)
(defun fbuild (n)
	"Build function f(i)->i*n"
	(lambda (x) (* x n))
	)
(defun double-arg (i)
	"Double arg"
	(funcall (fbuild i) 2)
	)
(double-arg 5)
 
(*** Exercice  4  ***)
(defun derivee (f a h)
	"Derivée au point a"
	(/ (- (funcall f (+ a h)) (funcall f a)) h)
	)
(derivee (lambda (x) (* x x x)) 1 0.0001)

(defun deriveen (f a n h)
	"Dérivée n-ième"
		(cond
			((zerop (1- n)) (derivee f a h))
			(t (/ (- (deriveen f (+ a h) (1- n) h) (deriveen f a (1- n) h)) h))
			)
	)
(deriveen (lambda (x) (* x x x)) 1 2 0.01)

(defun derivee2 (f a h)
	"Derivée2 au point a"
	(/ (- (funcall f (+ a h)) (funcall f (- a h)) (* 2 (funcall f a))) h)
	)

(*** Exercice  5  ***)
(defun sum-gen (op f n p)
	"Somme générale"
	(cond
		((= n p) (funcall f p))
		(t (funcall op (funcall f n) (sum-gen op f (1+ n) p)))
		)
	)
(sum-gen (lambda (x y) (+ x y)) (lambda (x) x) 1 5)

(defun factorielle (n)
	"Factorrielle"
	(sum-gen (lambda (x y) (* x y)) (lambda (x) x) 1 n)
	)
(factorielle 3)

(defun exponentielle (n)
	"e^n"
	(1+ (sum-gen (lambda (x y) (+ x y)) (lambda (i) (/ 1 (factorielle i))) 1 n))
	)
(exponentielle 10)

(*** Exercice  6  ***)
; (defun root-sum-prod (k)
; 	(labels ((root-sum (k))
; 		(if (< k 1)
; 			(0)
; 			(+ (sqrt k) (root-sum (-1 k))))
; 		))
; 	)

(*** Exercice  7  ***)
(funcall (lambda (x y) (+ (* 2 x) y )) 2 3)
(remove-if #'zerop '(0 1 0 2 0 3 0 0 0))
(remove-if-not (lambda (x) (= x 3)) '(0 1 2 3 0 1 2 3))
(mapcar (lambda (x) (* 2 x)) '(1 2 3))
 
(*** Exercice  8  ***)
(defun list-abs-mapcar (l)
	"Abs liste"
	(mapcar (lambda(x) (abs x)) l)
	)
(list-abs-mapcar '(-5 2 -3 -8 -10 2 4))

(defun list-square-mapcar (l)
	"Abs liste"
	(mapcar (lambda(x) (* x x)) l)
	)
(list-square-mapcar '(-5 2 -3 -8 -10 2 4))

(*** Exercice  9  ***)
(defun how-many (pred l)
	"Elems verify pred"
	(remove-if-not pred l)
	)
(how-many #'evenp '(1 2 3))
(how-many #'numberp '(2 3 4 a b 5 t + 8))

(*** Exercice  10  ***)
(defun insert (l k op)
	"Insertion"
	(cond 
		((null (car l)) (cons k '()))
		((funcall op (car l) k) (cons k l))
		(t (cons (car l) (insert (cdr l) k op))))
	)
(insert '(2 4 8) 1 #'>)

(defun sort-insert (l op &optional (acc '()))
	"Insertion sort"
	(cond
		((null (car l)) acc)
		(t (sort-insert (cdr l) op (insert acc (car l) op)))
		)
	)
(sort-insert '(8 5 7) #'<)

(*** Exercice  11  ***)
(defparameter matrix '((1 2 3) (4 5 6) (7 8 9)))

(defun transpose (m)
	"Transposée d'une matrice"
  	(cond ((null (car m)) nil)
		(t (cons (mapcar 'car m)
    	   (transpose (mapcar 'cdr m))))
	)
)
(apply #'mapcar #'list matrix)
(transpose matrix)

(defun scalar-product (u v &optional (s 0))
	"Produit scalaire de u et de v"
	(list (apply '+ (mapcar (lambda (x y) (* (car x) (car y))) u v)))
	)
(scalar-product '((1) (2) (3)) '((3) (2) (1)))

(defun mat-vect (u m &optional (r '()))
	"Produit vecteur u par matrice m"
	(cond
		((null (car (mapcar 'car m))) r)
		(t (mat-vect u (mapcar 'cdr m) (append r (scalar-product u (mapcar 'list (mapcar 'car m))))))
		)
	)
(mapcar (lambda (ligne) (scalar-product ligne u)) matrix)
(mat-vect '((1) (1) (1)) matrix)

(defun mat-mat (m1 m2)
	"Produit matrices"
	(mapcar (lambda (x) (mat-vect (transpose (list x)) m2)) m1)
	)
(mat-mat matrix matrix)

(*** Exercice  12  ***)
(defun map-and-sum (f l)
	"MAP & SUM"
	(apply '+ (mapcar f l))
	)
(map-and-sum 'abs '(-2 -3))

(defun map-and-prod (f l)
	"MAP & SUM"
	(apply '* (mapcar f l))
	)
(map-and-prod 'abs '(-2 -3))

(defun list-iterate (f l b)
	"Iterate apply f"
	(cond
		((null l) b)
		(t (funcall f (car l) (list-iterate f (cdr l) b)))
		)
	)
(list-iterate '+ '(1 2) 2)

(defun my-append (l1 l2)
	"Append clone"
	(list-iterate 'cons l1 l2)
	)
(my-append '(1 2) '(3 4))

(defun my-mapcar (f l)
	"Mapcar clone"
	(list-iterate (lambda (x y) (cons (funcall f x) y)) l NIL)
	)
(my-mapcar (lambda (x) (abs x)) '(-5 -6))

(defun prod-iterate (f l)
	"Prod a mapcar f l"
	(list-iterate (lambda (x y) (* (funcall f x) y)) l 1)
	)
(prod-iterate 'sqrt '(4 9 25))

(defun iterate-list (f l b)
	"Iterate list inverse"
	(cond
		((null l) b)
		(t (funcall f (iterate-list f (butlast l) b) (car (last l))))
		)
	)
(iterate-list '+ '(1 2) 2)

(defun my-reverse (l)
	"Reverse list"
	(iterate-list (lambda (x y) (cons y x)) l NIL)
	)
(my-reverse '(1 2 3))

(*** Exercice  13  ***) 
(defun append-map (f l)
  "returns the list (f(x1) f(x2) ... f(xn))"
  (apply #'append (mapcar f l)))
(append-map (lambda (x) (list x (* x x))) '(1 2 3 4)) ; --> (1 1 2 4 3 9 4 16)

(defun map-select (f L pred)
	"Map select"
	(append-map (lambda (x) (if (funcall pred x) (list (funcall f x)) NIL)) L)
	)

(map-select (lambda (x) (/ 1 x))
	    '(a 2 0 4 10)
	    (lambda (x) (and (numberp x) (not (zerop x))))) ; --> (1/2 1/4 1/10)

(defun my-remove-if (pred l)
	"Remove if pred"
	(append-map (lambda (x) (if (not (funcall pred x)) (list x) NIL)) l)
	)
(my-remove-if (lambda (x) (zerop x)) '(1 0 2 5 0))

(*** Exercice  14  ***)
