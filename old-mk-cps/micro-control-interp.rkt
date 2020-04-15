#lang racket

(define init-env
  (lambda (rns rbs)
    (map (λ (name exp)
           `(,name . ,(delay/name (ee exp (init-env rns rbs)))))
         rns
         rbs)))

(define (ep rns rbs q n)
  (let* ([re (init-env rns rbs)]
         [$ (ee q re)]
         [m$ (pull $)])
    (take n m$)))

(define (ee e re)
  (match e
    [`succeed '(#t)]
    [`fail '()]
    [`(disj ,g1 ,g2)
     (let* ([$2 (ee g2 re)]
            [$1 (ee g1 re)])
       ($append $1 $2))]
    [`(conj ,g1 ,g2)
     (let* ([$ (ee g1 re)])
       ($append-map $ g2 re))]
    [`(,r) (ae r re)]))

(define (ae r re) (cdr (assv r re)))

(define ($append $1 $2)
  (cond
    ((null? $1) $2)
    ((cons? $1) (cons (car $1) ($append (cdr $1) $2)))
    ((promise? $1) (delay/name ($append $2 (force $1))))))

(define ($append-map $ g re)
  (cond
    ((null? $) '())
    ((cons? $) ($append (ee g re) ($append-map (cdr $) g re)))
    ((promise? $) (delay/name ($append-map $ g re)))))

(define (pull $) (if (promise? $) (pull (force $)) $))

(define (take-inf n $)
  (cond
    ((null? $) '())
    ((and n (zero? (sub1 n))) (cons (car $) '()))
    (else (cons (car $) (take (and n (sub1 n)) (pull (cdr $)))))))

