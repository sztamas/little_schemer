(define atom?
  (lambda (x) (and (not (null? x)) (not (pair? x)))))

(define lat?
  (lambda (l)
    (cond 
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

(define rember
  (lambda (a l)
    (cond
      ((null? l) '())
      ((eq? (car l) a) (cdr l))
      (else (cons (car l) (rember a (cdr l)))))))

(define firsts
  (lambda (lol)
    (cond
      ((null? lol) '())
      (else (cons
              (car (car lol))
              (firsts (cdr lol)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) 
          (cons old (cons new (cdr lat))))
      (else 
          (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else 
            (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else
          (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else
          (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else 
        (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) 
          (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else
          (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
          (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else 
          (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
          (cons new (multisubst new old (cdr lat))))
      (else 
          (cons (car lat) (multisubst new old (cdr lat)))))))


;; Numbers Games

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(define tup?
  (lambda (x)
    (cond
      ((null? x) #t)
      ((not (list? x)) #f)
      (else 
          (cond
            ((number? (car x)) (tup? (cdr x)))
            (else #f))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else 
        (cons (o+ (car tup1) (car tup2))
            (tup+ (cdr tup1) (cdr tup2)))))))

(define gt
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (gt (sub1 n) (sub1 m))))))

(define lt
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (lt (sub1 n) (sub1 m))))))

(define eq
  (lambda (n m)
    (cond
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else (eq (sub1 n) (sub1 m))))))

(define my-expt
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (my-expt n (sub1 m)))))))

(define my-quotient
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (my-quotient (o- n m) m))))))

(define my-length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (my-length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else 
          (cons (car lat) 
              (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat))
          (no-nums (cdr lat)))
      (else 
          (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat))
          (cons (car lat) (all-nums (cdr lat))))
      (else 
          (all-nums (cdr lat))))))

(define eqan? 
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (eq a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else 
        (cond
          ((eq? (car lat) a)
            (add1 (occur a (cdr lat))))
          (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (eq n 1)))

;; Uses one? defined above
(define new-rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else 
        (cons (car lat) (rempick (sub1 n) (cdr lat)))))))


;; *Oh my Gawd*: It's Full of Stars

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? (car l) a) (rember* a (cdr l)))
          (else (cons (car l) (rember* a (cdr l))))))
      (else
          (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond 
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
            (cons old (cons new (insertR* new old (cdr l)))))
          (else 
            (cons (car l) (insertR* new old (cdr l)))))) 
      (else
        (cons 
          (insertR* new old (car l))
          (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
        (cond
          ((eq? (car l) a) (add1 (occur* a (cdr l))))
          (else (occur* a (cdr l)))))
      (else (o+
          (occur* a (car l))
          (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
            (cons new (subst* new old (cdr l))))
          (else
            (cons (car l) (subst* new old (cdr l))))))
      (else
        (cons
          (subst* new old (car l))
          (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? (car l) old)
            (cons new (cons old (insertL* new old (cdr l)))))
          (else
            (cons (car l) (insertL* new old (cdr l))))))
      (else
        (cons
          (insertL* new old (car l))
          (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
        (cond
          ((eq? (car l) a) #t)
          (else (member* a (cdr l)))))
      (else
        (or (member* a (car l))
            (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define my-equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
        (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
        (and (my-equal? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2)))))))

;; This version is more general. It removes S-Expression from a list of S-Expressions
;; instead an atom from a list of atoms
(define rember
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l)
              (rember s (cdr l)))))))


;; Shadows

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
        (and (numbered? (car aexp))
             (numbered? 
               (car (cdr (cdr aexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value 
  (lambda (nexp)
    (cond
        ((atom? nexp) nexp)
        ((eq? (operator nexp) (quote +))
          (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
        ((eq? (operator nexp) (quote x))
          (x (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
        ((eq? (operator nexp) (quote ^))
          (expt (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

;; value uses abstractions so we can switch to other representations of 
;; arithmetic expressions by just changing 1st-sub-exp, 2nd-sub-exp and operator
;; for example to work with a representation like this (3 + (4 ^ 5))
;; we just have to re-define 1st-sub-exp and operator

#|
(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))
|#

;; now we will represent numbers in a different way
;; () is zero
;; (()) is one
;; (() ()) is two
;; (() () ()) is three
;; ...

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define my-no+
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (new+ n (zub1 m)))))))

;; Bonus
(define my-no->number
  (lambda (n)
    (length n)))

;; (lat? '(1 2 3) is #t
;; BUT
;; (lat? '((()) (() ()) (() () ()))) is #f
;; You must beware of shadows!


