#lang racket

;; Registers
(define k 'hukarz)
(define $ 'hukarz)
(define $2 'hukarz)
(define g 'hukarz)
(define n 'hukarz)

;; Primitive Help Functions
(define (one? n) (eqv? n 1))
(define (monus1 n) (if (zero? n) n (sub1 n)))
(define (ae r) (cdr (assv r init-env)))
(define (delay? th) (eqv? (car th) 'th))

;; Continuation Constructors
(define (empty-k) `(empty-k))
(define (take-k k^) `(take-k ,k^))
(define (pull-k k^) `(pull-k ,k^))
(define ($app-proc-k $^ k^) `($app-proc-k ,$^ ,k^))
(define ($app-cons-k a^ k^) `($app-cons-k ,a^ ,k^))
(define ($app-map-proc-k g^ k^) `($app-map-proc-k ,g^ ,k^))
(define ($app-map-cons-k $^ g^ k^) `($app-map-cons-k ,$^ ,g^ ,k^))
(define (disj-k g^ k^) `(disj-k ,g^ ,k^))


;; Promise Constructors
(define ($app-th $1^ $2^) `(th $app-th ,$1^ ,$2^))
(define ($app-map-th $^ g^) `(th $app-map-th ,$^ ,g^))
(define (listo-th) `(th listo-th))
(define (unproductiveo-th) `(th unproductiveo-th))

;; Serious Functions 
(define (apply-k) ;; k $
  (match k
    [`(empty-k) $]
    [`(take-k ,k^)
     (begin 
       (set! k k^)
       (take))]
    [`(pull-k ,k^)
     (begin 
       (set! k k^)
       (pull))]
    [`($app-proc-k ,$^ ,k^)
     (begin 
       (set! k k^)
       (set! $2 $)
       (set! $ $^)
       ($app))]
    [`($app-cons-k ,a^ ,k^)
     (begin
       (set! k k^)
       (set! $ (cons a^ $))
       (apply-k))]
    [`($app-map-proc-k ,g^ ,k^)
     (begin
       (set! k k^)
       (set! g g^)
       ($app-map))]
    [`($app-map-cons-k ,$^ ,g^ ,k^) 
     (begin
       (set! k ($app-proc-k $ k^))
       (set! g g^)
       (set! $ $^)
       ($app-map))]
    [`(disj-k ,g^ ,k^)
     (begin
       (set! k ($app-proc-k $ k^))
       (set! g g^)
       (ee))]))

(define (force) ;; $ k 
  (match $
    [`(th $app-th ,$1^ ,$2^)
     (begin
       (set! k ($app-proc-k $2^ k))
       (set! $ $1^)
       (force))]
    [`(th $app-map-th ,$^ ,g^)
     (begin
       (set! k ($app-map-proc-k g^ k))
       (set! $ $^)
       (force))]
    [`(th listo-th)
     (begin
       (set! g '(disj (conj succeed succeed) (listo)))
       (ee))]
    [`(th unproductiveo-th)
     (begin
       (set! g '(unproductiveo))
       (ee))]))
    ;; (else ($ k*))

(define (ee) ;; g k
  (match g
    [`succeed
     (begin
       (set! $ '(#t))
       (apply-k))]
    [`fail
     (begin
       (set! $ '())
       (apply-k))]
    [`(disj ,g1 ,g2)
     (begin
       (set! k (disj-k g2 k))
       (set! g g1)
       (ee))]
    [`(conj ,g1 ,g2)
     (begin
       (set! k ($app-map-proc-k g2 k))
       (set! g g1)
       (ee))]
    [`(,r)
     (begin
       (set! $ (ae r))
       (apply-k))]))

(define ($app) ;; $ $2 k
  (cond
    ((null? $)
     (begin
       (set! $ $2)
       (apply-k)))
    ((delay? $)
     (begin
       (set! $ ($app-th $ $2))
       (apply-k)))
    ((cons? $)
     (begin
       (set! k ($app-cons-k (car $) k))
       (set! $ (cdr $))
       ($app)))))

(define ($app-map) ;; $ g k
  (cond
    ((null? $)
     (apply-k))
    ((delay? $)
     (begin
       (set! $ ($app-map-th $ g))
       (apply-k)))
    ((cons? $)
     (begin
       (set! k ($app-map-cons-k (cdr $) g k))
       (ee)))))

(define (pull) ;; $ k
  (if (delay? $)
      (begin
        (set! k (pull-k k))
        (force))
      (apply-k)))

(define (take) ;; n $ k
  (cond
    ((null? $)
     (apply-k))
    ((one? n)
     (begin
       (set! $ (cons (car $) '()))
       (apply-k)))
    (else
     (begin
       (set! k (take-k ($app-cons-k (car $) k)))
       (set! n (monus1 n))
       (set! $ (cdr $))
       (pull)))))

;; Initial Environment. 
(define init-env
  `((listo . ,(listo-th))
    (unproductiveo . ,(unproductiveo-th))))

;; Main
(begin
  (set! k (pull-k (take-k (empty-k))))
  (set! g '(listo))
  (set! n 5)
  (ee))


