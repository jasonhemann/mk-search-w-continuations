#lang racket
(require rackunit)
(require (submod "mk-streams-derivation.rkt" streams-unit-map-join))
;; (require (submod "mk-streams-derivation.rkt" streams-bind-return))
;; (require (submod "mk-streams-derivation.rkt" sk/fk-unit-map-join))
;; (require (submod "mk-streams-derivation.rkt" sk/fk-bind-return))

#| 

This should be the file in which we /use/ these various
implementations, and to show that they all work the same or
independent of one another, to get the right answers out.

|#

;; Obviously there are multiple ways to split this up.
;; Not sure what the "correct" one is, so I'm not.
;;

(test-begin 
  (define-relation (a n)
    (b n))
  (define-relation (b n)
    (c n))
  (define-relation (c n)
    (d n))
  (define-relation (d n)
    (unit n))

  (check-equal? '(5) (run (a 5)))
  
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

  (check-equal? '(5 5) (run 7 (l 5)))
  )

(test-begin 
  (define-relation (e n)
    (f n))
  (define-relation (f n)
    (g n))
  (define-relation (g n)
    (e n))

  (check-equal? '() (run 7 (e 5)))
  )


(test-begin 
  (define-relation (m n)
    (mplus (unit n) (m n)))

  (run 5 (mplus (m 5) (m 6)))

  (check-equal? 49 (run 50 (mplus (m 5) (m 6))))
  (check-equal? '(5 6 5 6) (run 5 (mplus (m 5) (m 6))))
)

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

  (loop* (mplus
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
            (d 1)))))


  )




