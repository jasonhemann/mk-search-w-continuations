#lang racket

(define rns '(listo))
(define rbs '((disj succeed (listo))))

(define init-env
  (map (λ (name exp)
         `(,name . ,(delay/name (ee exp))))
       rns
       rbs))

(define (ep q n)
  (let* ([$ (ee q)]
         [m$ (pull $)])
    (take n m$)))

(define (ee e)
  (match e
    [`succeed '(#t)]
    [`fail '()]
    [`(disj ,g1 ,g2)
     (let* ([$1 (ee g1)]
            [$2 (ee g2)])
       ($append $1 $2))]
    [`(conj ,g1 ,g2)
     (let* ([$ (ee g1)])
       ($append-map $ g2))]
    [`(,r) (ae r)]))

(define (ae r) (cdr (assv r init-env)))

(define ($append $1 $2)
  (cond
    ((null? $1) $2)
    ((cons? $1) (cons (car $1) ($append (cdr $1) $2)))
    ((promise? $1) (delay/name ($append $2 (force $1))))))

(define ($append-map $ g re)
  (cond
    ((null? $) '())
    ((cons? $) ($append (ee g) ($append-map (cdr $) g re)))
    ((promise? $) (delay/name ($append-map $ g re)))))

(define (pull $) (if (promise? $) (pull (force $)) $))

(define (take n $)
  (cond
    ((null? $) '())
    ((and n (zero? (sub1 n))) (cons (car $) '()))
    (else (cons (car $) (take (and n (sub1 n)) (pull (cdr $)))))))

