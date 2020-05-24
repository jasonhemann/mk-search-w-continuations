#lang racket
(require rackunit)

;; Any one of these 4
;; (require (submod "mk-streams-derivation.rkt" streams-unit-map-join))
;; (require (submod "mk-streams-derivation.rkt" streams-bind-return))
;; (require (submod "mk-streams-derivation.rkt" sk/fk-unit-map-join))
(require (submod "mk-streams-derivation.rkt" sk/fk-bind-return))

#| 

This should be the file in which we /use/ these various
implementations, and to show that they all work the same or
independent of one another, to get the right answers out.

|#

(test-begin 
  (define-relation (a n)
    (b n))
  (define-relation (b n)
    (c n))
  (define-relation (c n)
    (d n))
  (define-relation (d n)
    (unit n))

  (check-equal? '(5) (run (d 5)))
  (check-equal? '(5) (run (a 5)))

  (test-begin 
    (define-relation (e n)
      (f n))
    (define-relation (f n)
      (g n))
    (define-relation (g n)
      (e n))

    (check-equal? '() (run 7 (e 5)))
    (check-equal? '(5) (run 500 (mplus (e 5) (a 5))))
    )
  
  (define-relation (h n)
    (i n))
  (define-relation (i n)
    (j n))
  (define-relation (j n)
    (mplus (h n) (a n)))

  (check-equal? '(5 5) (run 20 (h 5)))

  (define-relation (l n)
    (m n))
  (define-relation (m n)
    (mplus (l n) (unit n)))

  (check-equal? '(5 5 5) (run 7 (l 5)))
  )

(test-begin 
  (define-relation (m n)
    (mplus (unit n) (m n)))

  (check-equal? 49 (length (run 50 (mplus (m 5) (m 6)))))
  (check-equal? '(5 6 5 6) (run 5 (mplus (m 5) (m 6))))
  )

(test-begin 
 (define-relation (l n)
   (mplus (unit n) (mplus (unit n) (l n))))

 ;; This indicates that we are measuring the number of pulls, not the number of answers.
 (check-equal? 98 (length (run 50 (mplus (l 5) (l 6)))))

 (define-relation (unproductiveo x)
   (unproductiveo x))

 (check-equal? '() (run 50 (unproductiveo 5))))

;; This seems promising, don't know if it's right.
(test-begin 
  (define-relation (a n)
    (mplus (unit 'a) (b (add1 n))))

  (define-relation (b n)
    (mplus (c (add1 n)) (unit 'b)))

  (define-relation (c n)
    (unit 'c))

  (define-relation (d n)
    (mplus (e (add1 n)) (unit 'd)))

  (define-relation (e n)
    (mplus (unit 'e) (f (add1 n))))

  (define-relation (f n)
    (unit 'f))

  (check-equal?
   '(v u c f e w a f b c d e b c f)
   (run (mplus
         (mplus
          (mplus
           (c 1)
           (unit 'u))
          (mplus
           (e 1)
           (mplus
            (a 1)
            (unit 'w))))
         (mplus
          (mplus
           (unit 'v)
           (f 1))
          (mplus
           (b 1)
           (d 1)))))))

(test-begin 
  (check-equal? '() (run ((bind (mzero)) (λ (n) (mzero)))))
  (check-equal? '() (run ((bind (mzero)) (λ (n) (unit 5)))))

  (check-equal? '()   (run ((bind (unit 5)) (λ (n) (mzero)))))
  (check-equal? '(25) (run ((bind (unit 5)) (λ (n) (unit (* n n))))))

  (test-begin 
    (define-relation (a n)
      (mplus (unit n) (b (add1 n))))

    (define-relation (b n)
      (mplus (unit n) (c (add1 n))))

    (define-relation (c n)
      (mplus (unit n) (d (add1 n))))

    (define-relation (d n)
      (unit n))

    (check-equal? '(25 26 27 28) (run ((bind (a 5)) (λ (n) (a (* n n))))))
    )
  
  (define-relation (nearly n)
    (unit n))

  (check-equal? '()   (run ((bind (nearly 5)) (λ (n) (mzero)))))
  (check-equal? '(25) (run ((bind (nearly 5)) (λ (n) (unit (* n n))))))
  )








