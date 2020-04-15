#lang racket
;; inline-fn call
;; pass init values of dk sk fk and c as arguments to looper
;; change looper to take

(define (ee g s dk)
  (match g
    [`(ore ,g1 ,g2)  (ee g1 s (λ (c) (ee g2 s (λ (c2) (dk (disj c c2))))))]
    [`(ande ,g1 ,g2) (ee g1 s) (λ (c) (dk (bind c g2)))]
    [`(succeed)      (dk (unit s))]
    [`(fail)         (dk (fail))]
    [`(alwayso)      (dk (mdelay (λ (dk) (ee '(ore (succeed) (alwayso)) s dk))))]))

;; not quite a c. It's a c missing a dk
(define (mdelay c)
  (λ (dk sk fk)
    (c dk)))

;; should we pass along this dk, or not? Could!
(define (unit s)
  (λ (dk sk fk)
    (sk s fk)))

(define (fail)
  (λ (dk sk fk)
    (fk)))

(define (disj c c2)
  (λ (dk sk fk)
    (c
     (λ (c) (dk (disj c2 c)))
     sk
     (λ () (c2 dk sk fk)))))

(define (bind c g)
  (λ (dk sk fk)
    (c 
     (λ (c) (dk (bind c g)))
     (λ (s fk) (ee g s (λ (c) (c dk sk fk)))) ;; can I just do this?
     fk)))

(define kons (λ (s fk) (cons s (fk))))
(define nill (λ () '()))
(define identity (λ (c) c))

;; computation starts from inside out, rather than outside in. 

(define init-dk 
  (λ (c) (c sk fk dk)))

(define (take n c sk fk init-dk)
  (match n
    [`Z (c sk fk init-dk)]
    [`(S ,n^) (c sk fk (λ (c) (take n^ c sk fk init-dk)))])) ;; Could instead nest up computations here

(take '(S Z) (ee '(ore (ande (succeed) (succeed)) (fail)) '() (λ (c) (c identity kons nill))))
(take '(S Z) (ee '(alwayso) '() (λ (c) (c identity kons nill))))
(take '(S (S (S (S (S (S (S (S (S (S Z)))))))))) (ee '(ore (alwayso) (ore (alwayso) (succeed))) '() (λ (c) (c identity kons nill))) identity kons nill)
(take '(S (S (S (S (S (S (S (S (S (S Z)))))))))) (ee '(ande (ore (alwayso) (succeed)) (succeed)) '() (λ (c) (c identity kons nill))) identity kons nill)
(take '(S (S (S (S (S (S (S (S (S (S Z)))))))))) (ee '(ande (ore (alwayso) (succeed)) (fail)) '() (λ (c) (c identity kons nill))) identity kons nill)

