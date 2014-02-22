(defun pgcd (a b)
	"Calcul du PGCD de a et b"
	(if (zerop b)
		a
	(pgcd b (mod a b)))
	)
(pgcd 2 0)

(defun somme-inter (x y)
	"Somme des nombres entre x et y"
	(cond
		((> x y) 0)
		((= x y) x)
		(t (+ x (somme-inter (1+ x) y)))
		)
	)
(somme-inter 1 5)

(defun square(x)
  "SQUARE of x"
  (* x x))

(defun somme-carre (x y)
	"Somme des nombres au carré entre x et y"
	(cond
		((> x y) 0)
		((= x y) (square x))
		(t (+ (square x) (somme-carre (1+ x) y)))
		)
	)
(somme-carre 0 3)

(defun puissance1 (x n)
	"Calcul de x^n, version 1"
	(cond
		((zerop n) 1)
		(t (* x (puissance1 x (1- n))))
		)
	)
(puissance1 2 2)

(defun puissance2 (x n)
	"Calcul de puissance, version 2"
	(cond
		((zerop n) 1)
		((zerop (mod n 2)) (square (puissance2 x (/ n 2))))
		(t (* x (puissance2 x (1- n))))
		)
	)
(puissance2 2 2)

(defun pair (n)
	"Le nombre est-il pair"
	(if (zerop n) t
		(impair (1- n)))
	)

(defun impair (n)
	"Le nombre est-il impair"
	(if (zerop n) NIL
		(pair (1- n)))
	)
(pair 2)
(pair 3)
(impair 2)
(impair 3)

(defun pair2 (n)
	"Le nombre est-il pair"
	(or (zerop n) (not (pair2 (1- n))))
	)

(defun impair2 (n)
	"Le nombre est-il impair"
	(and (not (zerop n)) (not (impair2 (1- n))))
	)
(pair2 2)
(pair2 3)
(impair2 2)
(impair2 3)

(defun syracuse (n)
	"Suite de Syracuse"
	(cond
		((= n 1) 0)
		((pair2 n) (1+ (syracuse (/ n 2))))
		(t (1+ (syracuse (1+ (* 3 n)))))
		)
	)
(syracuse 7)

(trace pair)
(trace impair)
(pair 10)

(trace pgcd)
(pgcd 3 5)

(untrace pair impair pgcd)

(defun plus (a b)
	"Addition récursive"
	(cond
		((zerop b) a)
		(t (1+ (plus a (1- b))))
		)
	)
(plus 1 521)

(defun produit (a b acc)
	"Produit récursif terminal"
	(cond
		((= 1 b) (+ a acc))
		(t (produit a (1- b) (+ acc a)))
		)
	)
(produit 2 3 0)

(untrace produit)