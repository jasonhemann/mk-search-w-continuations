#lang racket

(define (ee exp s)
  (match exp
    [`(ore ,g1 ,g2)  (disj (ee g1 s) (ee g2 s))]
    [`(ande ,g1 ,g2) (bind (ee g1 s) (λ (s) (ee g2 s)))]
    [`(succeed)      (unit s)]
    [`(fail)         (fail)]
    [`(alwayso)   (mdelay (ee `(ore (succeed) (alwayso)) s))]))

(define-syntax-rule (mdelay e)
  (λ (dk sk fk)
    (dk e)))

(define (unit a)
  (λ (dk sk fk)
    (sk a fk)))

(define (fail)
  (λ (dk sk fk)
    (fk)))

(define (disj c1 c2)
  (λ (dk sk fk)
    (c1 (λ (c1^) (dk (disj c2 c1^)))
        sk
        (λ () (c2 dk sk fk)))))

(define (bind m f)
  (λ (dk sk fk)
    (m 
     (λ (m^) (dk (bind m^ f)))
     (λ (b fk) ((f b) dk sk fk))
     fk)))

(define kons (λ (a fk) (cons a (fk))))
(define nill (λ () '()))
(define identity (λ (c) c))

(define (looper n c)
  (match n
    [`Z (c identity kons nill)]
    [`(S ,n^) (c (λ (c) (looper n^ c)) kons nill)]))

(looper '(S Z) (ee '(alwayso) '()))
(looper '(S (S (S (S (S (S (S (S (S (S Z)))))))))) (ee '(ore (alwayso) (ore (alwayso) (succeed))) '()))

