#lang racket

;; (define rns '(listo))
;; (define rbs '((disj succeed (listo))))
;; (define init-env
;;   (map (λ (name exp)
;;          `(,name . ,(λ (k) (ee exp k))))
;;        rns
;;        rbs))

(define (ep q n k)
  (ee q (ep-outer-k n k)))

(define (ee e k)
  (match e
    [`succeed (apply-k k '(#t))]
    [`fail (apply-k k '())]
    [`(disj ,g1 ,g2) (ee g1 (disj-outer-k g2 k))]
    [`(conj ,g1 ,g2) (ee g1 (conj-k g2 k))]
    [`(,r) (apply-k k (ae r))]))

(define (ae r) (cdr (assv r init-env)))

(define ($append $1 $2 k)
  (cond
    ((null? $1) (apply-k k $2))
    ((cons? $1) ($append (cdr $1) $2 ($append-cons-k $1 k)))
    ((procedure? $1) (apply-k k ($append-th $1 $2)))))

(define ($append-map $ g k)
  (cond
    ((null? $)      (apply-k k '()))
    ((procedure? $) (apply-k k ($append-map-th $ g)))
    ((cons? $)      (ee g ($append-map-cons-outer-k $ g k)))))

(define (pull $ k) 
  (cond
    ((null? $)      (apply-k k $))
    ((procedure? $) (force $ (pull-k k)))
    ((cons? $)      (apply-k k $))))

(define (take n $ k)
  (cond
    ((null? $)                (apply-k k '()))
    ((and n (zero? (sub1 n))) (apply-k k (cons (car $) '())))
    (else                     (pull (cdr $) (take-outer-k n $ k)))))


(define (apply-k k v)
  (match k
    (else (k v))))

(define (ep-inner-k n k)
  (λ (m$)
    (take n m$ k)))

(define (ep-outer-k n k)
  (λ ($)
    (pull $ (ep-inner-k n k))))

(define (disj-inner-k $1 k)
  (λ ($2) 
    ($append $1 $2 k)))

(define ($append-proc-force-k $2 k^)
  (λ ($1^)
    ($append $2 $1^ k^)))

(define (disj-outer-k g2 k)
  (λ ($1)
    (ee g2 (disj-inner-k $1 k))))

(define (conj-k g2 k)
  (λ ($)
    ($append-map $ g2 k)))

(define ($append-cons-k $1 k)
  (λ ($r)
    (apply-k k (cons (car $1) $r))))

(define ($append-map-cons-inner-k $v k)
  (λ ($w)
    ($append $v $w k)))

(define ($append-map-cons-outer-k $ g k)
  (λ ($v)
    ($append-map (cdr $) g ($append-map-cons-inner-k $v k))))

(define ($append-map-proc-k g k)
  (λ ($^)
    ($append-map $^ g k)))

(define (take-inner-k $ k)
  (λ (res)
    (apply-k k (cons (car $) res))))

(define (take-outer-k n $ k)
  (λ (m$)
    (take (and n (sub1 n)) m$ (take-inner-k $ k))))

(define (empty-k)
  (λ (v) v))

(define (pull-k k)
  (λ (v)
    (pull v k)))

(define ($append-th $1 $2)
  (λ (k^)
    (force $1 ($append-proc-force-k $2 k^))))

(define ($append-map-th $ g)
  (λ (k) 
    (force $ ($append-map-proc-k g k))))

(define (listo-th)
  (λ (k) 
    (ee '(disj (conj succeed succeed) (listo)) k)))

(define (unproductiveo-th)
  (λ (k) 
    (ee '(unproductiveo) k)))

(define (force $ k)
  (match $
    (else ($ k))))

(define init-env `((listo . ,(listo-th)) (unproductiveo . ,(unproductiveo-th))))

(ep '(listo) 5 (empty-k))


