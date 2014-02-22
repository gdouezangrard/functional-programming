(*** Exercice  1  ***)
 
(cons '(A B C) '(1 2 3)) ;; -> ((A B C) 1 2 3)
(append '(A B C) '((1 2) 3)) ;; -> (A B C (1 2) 3)
(last '((A 1) (B 2) (C 3))) ;; -> ((C 3))
(butlast '((A 1) (B 2) (C 3))) ;; -> ((A 1) (B 2))
(car '((A (B C)) D (E F))) ;; -> (A (B C))
(cdr '((A (B C)) D (E F))) ;; -> (D (E F))
(caddr'((A (B C)) D (E F))) ;; -> (E F)
(cons 'NOBODY (cons 'IS '(PERFECT))) ;; -> (NOBODY IS PERFECT)
(list (1+ 2) (1- 5) 6) ;; -> (3 4 6)
(cdr '(a b)) ;; -> (B)
(cdr '(a . b)) ;; -> B
'(a . (b . (c . ())))
'(a . (b . (c . d)))
(assoc 'bleu '((rouge . red) (vert . green) (bleu . blue) (jaune . pink)))
 
(*** Exercice  2  ***)
 
;; Replace '?' character to retrieve the right element of the list
(cadddr '(A B C D)) ;; -> D
(cdadar '((A (B C)) E)) ;; -> C
(caaar '(((DIEU) ENCORE) UNE)) ;; -> DIEU
(cdar '(((DIEU) ENCORE) UNE)) ;; -> ENCORE

(car '(foo))
(caadr '(bar (foo)))
(cadar '((bar foo)))
(caddr '(bar baz foo))
(caar (cdaadr '(bar ((baz (foo))))))
(caadr (cadaar '(((bar (baz (foo)))))))
 
(*** Exercice  3  ***)
(defun inversion (l)
	"Inversion d'une liste à 3 éléments"
	(cons (caddr l) (cons (cadr l) (cons (car l) '())))
	)

(inversion '(1 2 3))

(defun inversion2 (l)
	"Inversion v2"
	(cons (third l) (cons (second l) (cons (first l) '())))
	)

(inversion2 '(1 2 3))

(cons 
	(car '(1 2))
	(cons
		(cadr '(1 2))
		(cons
			(caar '((3 4)))
			(cons
				(cadar '((3 4)))
				(cons
					5
					'()
				)
			)
		)
	)
)

(append
	'(1 2)
	(append
		'(3 4)
		'(5)
	)
)


(*** Exercice  4  ***)
 
(defparameter liste1 '(a b c))
(defparameter liste2 '(b c))
(defparameter liste3 (cons 'a liste2))

(eq (cdr liste1) liste2)
(eq (cdr liste3) liste2)
(equal (cdr liste1) liste2)
 
(*** Exercice  5  ***)
(defun list-abs-recursif (l)
	"Map |i| for i in l"
	(if (equal l '())
		NIL
		(cons
			(abs (car l))
			(list-abs-recursif (cdr l))
			)
		)
	)

(list-abs-recursif '(-1 2 -6))
 
(*** Exercice  6  ***)
(defun repeat (e n)
	"Build (e e e ... e) n times"
	(if (= n 0)
		NIL
		(cons
			e
			(repeat e (1- n))
			)
		)
	)

(repeat 'glou 7)

 
(*** Exercice  7  ***)
(defun swap-first-last (l)
	"Swap first elem with the last"
	(append
		(last l)
		(append
			(butlast (cdr l))
			(cons (car l) '())
			)
		)
	)

(swap-first-last '(YOU CANT BUY LOVE))
 
(*** Exercice  8  ***)
(defun rotate-left (l n)
	"Circular rotation to left"
	(if (> n 0)
		(rotate-left (cons (car (last l)) (butlast l)) (1- n))
		l
		)
	)

(rotate-left '(1 2 3) 1)

(defun rotate-right (l n)
	"Circular rotation to right"
	(if (> n 0)
		(rotate-right (append (cdr l) (cons (car l) '())) (1- n))
		l
		)
	)

(rotate-left '(1 2 3) 1)
 
(*** Exercice  9  ***)
(defun insert (l k)
	"Insertion"
	(cond 
		((null (car l)) (cons k '()))
		((> (car l) k) (cons k l))
		(t (cons (car l) (insert (cdr l) k))))
	)
(insert '(2 4 8) 1)

(defun sort-insert (l &optional (acc '()))
	"Insertion sort"
	(cond
		((null (car l)) acc)
		(t (sort-insert (cdr l) (insert acc (car l))))
		)
	)
(sort-insert '(8 5 7))

(defun insert-symbol (l k)
	"Insertion symbol"
	(cond 
		((null (car l)) (cons k '()))
		((string> (car l) k) (cons k l))
		(t (cons (car l) (insert-symbol (cdr l) k))))
	)
(insert-symbol '(test yolo michou) 'abriel)
(insert-symbol '(test yolo michou) 'zebra)

(defun sort-insert-symbol (l &optional (acc '()))
	"Insertion sort symbols"
	(cond
		((null (car l)) acc)
		(t (sort-insert-symbol (cdr l) (insert-symbol acc (car l))))
		)
	)
(sort-insert-symbol '(galadriel gandalf sauron michel))

(defun scinder (l)
	"Scinder liste"
	(cond
		((<= (list-length l) 1) (cons l '()))
		(t (let ((liste (scinder (cddr l))))
			(cons (cons (car l) (car liste)) (cons (cons (cadr l) (cadr liste)) '()))
			)
		))
	)
(scinder '(1 2 3 5 6 7))

(defun fusionner (l1 l2)
	"Fusionner 2 listes"
	(cond
		((null l1) l2)
		((null l2) l1)
		(t (if (<= (car l1) (car l2))
			(cons (car l1) (fusionner (cdr l1) l2)) (cons (car l2) (fusionner l1 (cdr l2)))))
		)
	)

(fusionner '(1 2 5) '(2 4 5))

(defun tri-fusion (l)
	"Tri fusion"
	(cond
		((<= (list-length l) 1) l)
		(t (let* (
			(liste (scinder l))
			(liste1 (car liste))
			(liste2 (cadr liste))
			)
			(fusionner (tri-fusion liste1) (tri-fusion liste2))
			)
		)
	)
	)
(tri-fusion '(2 1 9 5))

(*** Exercice  10  ***)
(defun proper-list (l)
	"Est liste propre"
	(or 
		(null l)
		(and (consp l) (proper-list (cdr l)))
		)
	)
(proper-list '(1 2 3))
(proper-list (cons 1 2))

(defun pointless-list-p (l)
	"List without points"
	(or 
		(null l)
		(and (consp l) (atom (car l)) (pointless-list-p (cdr l)))
		(and (consp l) (pointless-list-p (car l)) (pointless-list-p (cdr l)))
		)
	)
(proper-list '(1 2 3))
(proper-list (cons 1 2))

(*** Exercice  11  ***)
(defun iota (n &optional (l '()))
	"renvoie (1 2 3 ... n)"
	(cond
		((zerop n) l)
		(t (iota (1- n) (cons n l)))
		)
	)

(iota 5)

(defun sum-list (l &optional (a 0))
	"Somme de termes d'une liste"
	(cond
		((null l) a)
		(t (sum-list (cdr l) (+ a (car l))))
		)
	)
(sum-list '(1 2 5))

(defun scalar-product (l1 l2 &optional (a 0))
	"Produit scalaire de deux listes DE MÊMES TAILLES !"
	(cond
		((null l1) a)
		(t (scalar-product (cdr l1) (cdr l2) (+ a (* (car l1) (car l2)))))
		)
	)
(scalar-product '(1 2) '(4 -2))

(defun divisors (n &optional (a '()) (i 1))
	"Liste de diviseurs"
	(cond 
		((> i n) a)
		((zerop (mod n i)) (divisors n (cons i a) (1+ i)))
		(t (divisors n a (1+ i)))
		)
	)

(divisors 6)

(*** Exercice  12  ***)
(defun flatten (l)
	"Liste plate"
	(cond
		((null l) l)
		((atom l) (list l))
		(t (append (flatten (car l)) (flatten (cdr l))))
		)
	)
(flatten '(1 (2 (3 . 5)) 4))

(defun mc-flatten1 (e l)
	"Mc Carthy 1"
	(append (flatten e) l)
	)

(defun mc-flatten (e &optional (l '()))
	"Mc Carthy !"
	(cond
		((null e) l)
		((atom (car e)) (mc-flatten (cdr e) (append l (list (car e)))))
		(t (mc-flatten (append (car e) (cdr e)) l))
		)
	)
(mc-flatten '(1 ((2 (3)))))
 
(*** Exercice  13  ***)
(defun reverse-list (l &optional (a '()))
	"Reverse list top elements"
	(cond
		((null l) a)
		((atom l) l)
		(t (reverse-list (cdr l) (cons (car l) a)))
		)
	)
(reverse-list '(1 2 (3 2)))

(defun reverse-list*-aux (l &optional (a '()))
	"Reverse list rec elements AUX"
	(cond
		((null l) (list a))
		((atom l) (list l))
		(t (reverse-list*-aux (cdr l) (append (reverse-list*-aux (car l) '()) a)))
		)
	)

(defun reverse-list* (l)
	"Reverse list rec elements AUX"
	(car (reverse-list*-aux l))
	)
(reverse-list* '(1 2 (3 2)))
 
(*** Exercice  14  ***)
(defun stutter (l &optionnal (r '()))
	"Bégaiement"
	(cond 
		((null l) r)
		()
		)
	)