#lang racket

(define (bind c f)
  (λ (sk)
    (λ (fk)
      (c (λ (a/fk)
           (λ (fk)
            (if (a? a/fk)
                (((f a/fk) sk) fk)
                (fk (λ (a/fk^)
                      (bind (a/fk a/fk^) f))))))
         fk))))

(define (disj c1 c2)
  (λ (sk)
    (λ (fk)
      ((c1
        (λ (a/fk)
          (λ (fk)
            (if (a? a/fk)
                ((sk a/fk) fk)
                (fk a/fk)))))
       (λ (fk^) ((c2 sk) (fk fk^)))))))

(define (allo)
  (λ (sk)
    (λ (fk)
      (sk (λ (a/fk^)
            (((disj S (allo))
              ...)
             fk))
          fk))))

(define S
  (λ (sk)
    (λ (fk)
      ((sk #t) fk))))

(define F
  (λ (sk)
    (λ (fk)
      (fk '()))))



