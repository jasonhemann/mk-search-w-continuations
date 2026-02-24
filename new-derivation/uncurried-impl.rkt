#lang racket
(require rackunit)
(provide (all-defined-out))

(define (run . args)
  (cond
    ((null? (cdr args)) (loop* (car args)))
    (else (loop (car args) (cadr args)))))

(define (loop n c)
  (c (λ (c^)
       (if (zero? n)
           '()
           (loop (sub1 n) c^)))
     (λ (a c)
       (cons a (loop n c)))
     (λ () '())))

(define (loop* c)
  (c loop*
     (λ (a c)
       (cons a (loop* c)))
     (λ () '())))

(define-syntax-rule (define-relation (n . args) g)
  (define (n . args)
    (λ (dk sk fk)
      (dk g))))

(define-syntax-rule (freeze e) 
  (λ (dk sk fk)
    (dk e)))

(define (return a)
  (λ (dk sk fk)
    (sk a (mzero))))

(define (bind m f)
  (λ (dk sk fk)
    (m (λ (m^)
         (dk (bind m^ f)))
       (λ (b c)
         ((mplus (f b) (bind c f))
          dk sk fk))
       fk)))

(define (mzero)
  (λ (dk sk fk)
    (fk)))

(define (mplus m1 m2)
  (λ (dk sk fk)
    (m1 (λ (c1^)
          (dk (mplus m2 c1^)))
        (λ (a c) 
          (sk a (mplus c m2)))
        (λ ()
          (m2 dk sk fk)))))

(define (unit a)
  (return a))

(define (map f m)
  (bind m (λ (a) (unit (f a)))))

(define (join z)
  (bind z (λ (a) a)))

