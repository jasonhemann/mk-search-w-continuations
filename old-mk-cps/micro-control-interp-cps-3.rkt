#lang racket
(define (empty-k) `(empty-k))
(define (disj-outer-k g2^ k^) `(disj-outer-k ,g2^ ,k^))
(define (conj-k g2^ k^) `(conj-k ,g2^ ,k^))
(define (pull-k k^) `(pull-k ,k^))
(define (take-k n^ k^) `(take-k ,n^ ,k^))
(define ($append-cons-k $1 k) `($append-cons-k ,$1 ,k))
(define ($append-proc-inner-k $2 k^) `($append-proc-inner-k ,$2 ,k^))
(define ($append-proc-outer-k $1 $2) `($append-proc-outer-k ,$1 ,$2))
(define ($append-map-cons-outer-k $ g k) `($append-map-cons-outer-k ,$ ,g ,k))
(define ($append-map-proc-k $ g) `($append-map-proc-k ,$ ,g))

(define (one? n) (eqv? n 1))
(define (monus1 n) (if (zero? n) 1 (sub1 n)))
(define (ae r) (cdr (assv r init-env)))

;; (define rns '(listo))
;; (define rbs '((disj succeed (listo))))
;; (define init-env
;;   (map (λ (name exp)
;;          `(,name . ,(λ (k) (ee exp k))))
;;        rns
;;        rbs))

(define init-env `((listo . ,(λ (k) (ee '(disj (conj succeed succeed) (listo)) k)))))

(define (apply-k k v)
  (match k
    [`(empty-k)                            v]
    [`(disj-outer-k ,g2^ ,k^)              (ee g2^ ($append-proc-inner-k v k^))]
    [`(pull-k ,k^)                         (pull v k^)]
    [`(take-k ,n^ ,k^)                     (take n^ v k^)]
    [`($append-cons-k ,a ,k)               (apply-k k (cons a v))]
    [`($append-proc-outer-k ,$1 ,$2)       (apply-th $1 ($append-proc-inner-k $2 v))]
    [`($append-proc-inner-k ,$ ,k^)        ($append $ v k^)]
    [`($append-map-cons-outer-k ,$ ,g ,k^) ($append-map $ g ($append-proc-inner-k v k^))]
    [`($append-map-proc-k ,$ ,g)           ($append-map $ g v)]
    [`(conj-k ,g ,k^)                      ($append-map v g k^)]))

(define (ee e k)
  (match e
    [`succeed        (apply-k k '(#t))]
    [`fail           (apply-k k '())]
    [`(disj ,g1 ,g2) (ee g1 (disj-outer-k g2 k))]
    [`(conj ,g1 ,g2) (ee g1 (conj-k g2 k))]
    [`(,r)           (apply-k k (ae r))]))

(define ($append $1 $2 k)
  (cond
    ((null? $1)      (apply-k k $2))
    ((cons? $1)      ($append (cdr $1) $2 ($append-cons-k (car $1) k)))
    ((procedure? $1) (apply-k k ($append-proc-outer-k $1 $2)))))

(define (apply-th $ k) ($ k))

(define ($append-map $ g k)
  (cond
    ((null? $)      (apply-k k '()))
    ((cons? $)      (ee g ($append-map-cons-outer-k (cdr $) g k)))
    ((procedure? $) (apply-k k ($append-map-proc-k $ g)))))

(define (pull $ k) 
  (cond
    ((null? $)      (apply-k k $))
    ((cons? $)      (apply-k k $))
    ((procedure? $) (apply-th $ (pull-k k)))))

(define (take n $ k)
  (cond
    ((null? $) (apply-k k '()))
    ((one? n)  (apply-k k (cons (car $) '())))
    (else      (pull (cdr $) (take-k (monus1 n) ($append-cons-k (car $) k))))))

(ee '(listo) (pull-k (take-k 5 (empty-k))))
