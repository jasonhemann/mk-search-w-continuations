#lang racket 

(define (unit a)
  (λ (dk) ;; this begins a computation
    (λ (sk) (sk a))))

(define fail
  (λ (dk) ;; this begins a computation
    (λ (sk)
      (λ (fk) (fk)))))

(define (star f g)
  (λ (sk)
    (λ (fk) ((f sk) (λ () ((g sk) fk))))))

(define ((disj c1) c2) ;; these two arguments are computations
  (λ (dk) ;; this begins a computation
    (star (c1 (λ (c1^) (dk ((disj c2) c1^) ;; this disj is also a computation
                           )))
          (c2 dk))))

(define ((bind m) f)
  (λ (dk) ;; this begins a computation
    (λ (sk)
      ( (m (λ (m^) (dk ((bind m^) f) ;; this bind is also a computation
                      )))
       (λ (b) (((f b) ;; this (f b) is also a computation
                dk) sk))))))

(define (map f m^) ;; of course m^ is a computation
  ((bind m^) (λ (a) (unit (f a)) ;; this unit expression is also a computation
                ))) ;; the expression ((bind m^) (λ (a) (unit (f a)))) is a computation

(define (join mm^) ;; mm^ is a computation. It's a nested one.
  ((bind mm^) identity)) ;; the expression ((bind mm^) identity) is a computation

(define kons (λ (a) (λ (fk) (cons a (fk)))))
(define nill (λ () '()))
(define identity (λ (c) c))

(define (looper n)
  (if (zero? n) 
      (λ (x) (((x identity) kons) nill)) ;; this x is a computation
      (λ (x) (((x (looper (sub1 n))) kons) nill)))) ;; this x is a computation.

(define-syntax-rule (define-relation (n . args) b)
  (define ((((n . args) dk) sk) fk) (dk (b args))))
