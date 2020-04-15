#lang racket

;; (define rns '(listo))
;; (define rbs '((disj succeed (listo))))
;; (define init-env
;;   (map (λ (name exp)
;;          `(,name . ,(λ (k) (ee exp k))))
;;        rns
;;        rbs))

(define init-env `((listo . ,(λ (k) (ee '(disj (conj succeed succeed) (listo)) k)))))

(define (empty-k) `(empty-k))
(define (ep-inner-k n^ k^) `(ep-inner-k ,n^ ,k^))
(define (ep-outer-k n^ k^) `(ep-outer-k ,n^ ,k^))
(define (disj-inner-k $1^ k^) `(disj-inner-k ,$1^ ,k^))
(define (disj-outer-k g2^ k^) `(disj-outer-k ,g2^ ,k^))
(define (conj-k g2^ k^) `(conj-k ,g2^ ,k^))
(define (pull-k k^) `(pull-k ,k^))
(define (take-inner-k $^ k^) `(take-inner-k ,$^ ,k^))
(define (take-outer-k n^ $^ k^) `(take-outer-k ,n^ ,$^ ,k^))
(define ($append-cons-k $1 k) `($append-cons-k ,$1 ,k))
(define ($append-proc-inner-k $2 k^) `($append-proc-inner-k ,$2 ,k^))
(define ($append-proc-outer-k $1 $2) `($append-proc-outer-k ,$1 ,$2))
(define ($append-map-cons-inner-k $ k) `($append-map-cons-inner-k ,$ ,k))
(define ($append-map-cons-outer-k $ g k) `($append-map-cons-outer-k ,$ ,g ,k))
(define ($append-map-proc-k $ g) `($append-map-proc-k ,$ ,g))

(define (apply-k k v)
  (match k
    [`(empty-k)                           v]
    [`(disj-outer-k ,g2^ ,k^)             (ee g2^ (disj-inner-k v k^))]
    [`(pull-k ,k^)                        (pull v k^)]
    [`(ep-outer-k ,n^ ,k^)                (pull v (ep-inner-k n^ k^))]
    [`(ep-inner-k ,n^ ,k^)                (take n^ v k^)]
    [`(take-outer-k ,n^ ,$^ ,k^)          (take (and n^ (sub1 n^)) v (take-inner-k $^ k^))]
    [`(take-inner-k ,$^ ,k^)              (apply-k k^ (cons (car $^) v))]
    [`($append-cons-k ,$1 ,k)             (apply-k k (cons (car $1) v))]
    [`($append-proc-outer-k ,$1 ,$2)      (force $1 ($append-proc-inner-k $2 v))]
    [`(disj-inner-k ,$1^ ,k^)             ($append $1^ v k^)]
    [`($append-map-cons-inner-k ,$ ,k)    ($append $ v k)]
    [`($append-proc-inner-k ,$2 ,k^)      ($append $2 v k^)]
    [`($append-map-cons-outer-k ,$ ,g ,k) ($append-map (cdr $) g ($append-map-cons-inner-k v k))]
    [`($append-map-proc-k ,$ ,g)          ($append-map $ g v)]
    [`(conj-k ,g2^ ,k^)                   ($append-map v g2^ k^)]))

(define (ep q n k)
  (ee q (ep-outer-k n k)))

(define (ee e k)
  (match e
    [`succeed (apply-k k '(#t))]
    [`fail (apply-k k '())]
    [`(disj ,g1 ,g2)
     (ee g1 (disj-outer-k g2 k))]
    [`(conj ,g1 ,g2)
     (ee g1 (conj-k g2 k))]
    [`(,r) (apply-k k (ae r))]))

(define (ae r) (cdr (assv r init-env)))

(define ($append $1 $2 k)
  (cond
    ((null? $1) (apply-k k $2))
    ((cons? $1)
     ($append (cdr $1) $2 ($append-cons-k $1 k)))
    ((procedure? $1)
     (apply-k k ($append-proc-outer-k $1 $2)))))

(define (force $ k) ($ k))

(define ($append-map $ g k)
  (cond
    ((null? $) (apply-k k '()))
    ((cons? $) (ee g ($append-map-cons-outer-k $ g k)))
    ((procedure? $) (apply-k k ($append-map-proc-k $ g)))))

(define (pull $ k) 
  (if (procedure? $)
      (force $ (pull-k k))
      (apply-k k $)))

(define (take n $ k)
  (cond
    ((null? $) (apply-k k '()))
    ((and n (zero? (sub1 n))) 
     (apply-k k (cons (car $) '())))
    (else
     (pull (cdr $) (take-outer-k n $ k)))))

(ep '(listo) 5 (empty-k))
