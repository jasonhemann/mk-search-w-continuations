#lang racket

(define (unit a)
  (λ (dk sk fk)
    (sk a fk)))

(define fail
  (λ (dk sk fk)
    (fk)))

(define (disj c1 c2)
  (λ (dk sk fk)
    (c1 (λ (c1) (dk (disj c2 c1)))
        sk
        (λ () (c2 dk sk fk)))))

(define (bind m f)
  (λ (dk sk fk)
    (m (λ (m) (dk (bind m f)))
       (λ (a fk) ((f a) dk sk fk))
       fk)))

;; (define (map f m^)
;;   (bind m^ (λ (a) (unit (f a)))))

;; (define (join mm^)
;;   (bind mm^ identity))

(define kons (λ (a fk) (cons a (fk))))
(define nill (λ () '()))
(define identity (λ (c) c))

(define (looper n)
  (if (zero? n)
      (λ (x) (x identity kons nill))
      (λ (x) (x (looper (sub1 n)) kons nill))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (a n)
  (λ (dk sk fk)
    (dk (b n))))

(define (b n)
  (λ (dk sk fk)
    (dk (c n))))

(define (c n)
  (λ (dk sk fk)
    (dk (disj (unit n) (a (add1 n))))))

(define (d n)
  (λ (dk sk fk)
    (dk (e n))))

(define (e n)
  (λ (dk sk fk)
    (dk (disj (unit n) (d (add1 n))))))


((looper 0) (disj (unit 5) fail))
((looper 10) fail)
((looper 30) (disj (a 20) (d 5)))
((looper 30) (bind (a 5) b))
