(defun plus (a b)
	"Addition récursive"
	(cond
		((zerop b) a)
		(t (plus (+ a 1) (1- b)))
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

(defun fibo (n)
	"Suite fibonacci"
	(cond
		((= n 0) 1)
		((= n 1) 1)
		(t (+ (fibo (1- n)) (fibo (- n 2))))
		)
	)
(fibo 4)

(defun fibo-gen (n a b)
	"Aux fibo"
	(cond
		((= n 0) a)
		((= n 1) b)
		(t (+ (fibo-gen (- n 1) a b) (fibo-gen (- n 2) a b)))
		)
	)
(fibo-gen 4 1 1)

(defun fibo-gen2 (n a b)
	"Aux fibo"
	(cond
		((= n 0) a)
		((= n 1) b)
		(t (fibo-gen2 (- n 1) b (+ a b)))
		)
	)
(fibo-gen2 32 1 1)

(time (fibo 32))
(time (fibo-gen 32 1 1))
(time (fibo-gen2 32 1 1))

(defun test-arret? (a x eps)
	"|e²-x| <= eps"
	(cond
		((<= (abs (- (* a a) x)) eps) t)
		(t NIL)
		)
	)

(defun suivant (a x)
	"Terme suivant de la série"
	(/ (+ a (/ x a)) 2)
	)

(defun racine-rec (start x eps)
	"Racine carrée"
	(cond
		((test-arret? start x eps) start)
		(t (racine-rec (suivant start x) x eps))
		)
	)

(defun racine-carre (x)
	"Calcul racine carrée"
	(racine-rec 1 x 0.000001)
	)
(racine-carre 4)


(defun test-arret?3 (a x eps)
	"|e^3-x| <= eps"
	(cond
		((<= (abs (- (* a a a) x)) eps) t)
		(t NIL)
		)
	)

(defun suivant3 (a x)
	"Terme suivant de la série3"
	(/ (+ (* 2 a) (/ x (* a a))) 3)
	)

(defun racine3-rec (start x eps)
	"Racine carrée"
	(cond
		((test-arret?3 start x eps) start)
		(t (racine3-rec (suivant3 start x) x eps))
		)
	)

(defun racine-cubique (x)
	"Calcul racine carrée"
	(racine3-rec 1 x 1)
	)
(racine-cubique 8)

(let ((x 5)
	(y (sqrt 9))
	(z (* 4 (+ 3 (sqrt 4) (sqrt 9)))))
	(+ x y (* 5 z)))

(+ (let ((a 12)) (+ a a))
	(let ((b 30)) (+ b
		(let ((b 20))
		(+ b b)))))

(defparameter x 12)
(defparameter y 5)

(defparameter q1 
	(let 
	((x y)
	(y x))
	(- x y)
	)
)
(+ q1 0)

(defparameter q2 (let* ((x y)
(y x))
(- x y)))
(+ q2 0)

(defun print-base (n base)
	(let (
		( *print-base* base))
		(print n)
		)
	)
(print-base 6 2)

(defparameter *n-fois* 128)
(defun g (*n-fois* x)
	(if (zerop *n-fois*)
		0
	(f x)))

(defun f (x)
	(* *n-fois* x))

(g 2 3)
(print *n-fois*)

(defun new-if (predicate clause-then clause-else)
	(if predicate
		clause-then clause-else)
	)

(defparameter a 0)
(new-if (zerop a) t nil)
(new-if (zerop a)
(print "a␣est␣nul")
(print "a␣est␣non␣nul"))

(defparameter a 20)
(and (print 1) (= a 20) (print 2) (print 3))
(and (print 1) (= a 30) (print 2) (print 3))
(or (print 1) (= a 20) (print 2) (print 3))
(or (= a 30) (= a 50) (print 1) (print 2))