#lang racket

(define (((ee g) sk) fk)
  (match g
   [`s ((sk #t) fk)]
   [`f (fk)]
   [`(d ,g1 ,g2) (((ee g1) sk) (λ () (ee g2 sk fk)))]
   [`(c ,g1 ,g2) (((ee g1) (λ (s) (ee g2 sk fk))) fk)]
   [`(prod) (((ee '(d s (prod))) sk) fk)]))

(((ee '(d (d (c s s) f) s)) (λ (v) (λ (fk) v))) (λ () '()))
(((ee '(prod)) (λ (v) (λ (fk) v))) (λ () '())) 




