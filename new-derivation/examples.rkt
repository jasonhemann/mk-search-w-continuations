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

(test-begin 
  (define-relation (a n)
    (b n))
  (define-relation (b n)
    (c n))
  (define-relation (c n)
    (d n))
  (define-relation (d n)
    (unit n))

  (check-pred promise? (a 5))
  (check-pred promise? (force (a 5))) 
  (check-pred promise? (force (force (a 5)))) 
  (check-pred promise? (force (force (force (a 5)))))
  (check-equal? '(5) (force (force (force (force (a 5))))))
  
  (define-relation (h n)
    (i n))
  (define-relation (i n)
    (j n))
  (define-relation (j n)
    (mplus (h n) (a n)))

  (check-pred promise? (force (force (force (force (force (force (force (h 5)))))))))
  (check-pred promise? (force (force (force (force (force (force (force (force (h 5))))))))))
  (check-pred promise? (force (force (force (force (force (force (force (force (force (h 5)))))))))))
  (check-pred promise? (force (force (force (force (force (force (force (force (force (force (force (h 5)))))))))))))
  (check-match (force (force (force (force (force (force (force (force (force (force (force (force (h 5))))))))))))) (cons 5 x) (promise? x))  
  (check-pred promise? (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5))))))))))))))))
  (check-pred promise? (force (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5)))))))))))))))))
  (check-pred promise? (force (force (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5))))))))))))))))))
  (check-pred promise? (force (force (force (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5)))))))))))))))))))
  (check-pred promise? (force (force (force (force (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5))))))))))))))))))))
  (check-pred promise? (force (force (force (force (force (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5)))))))))))))))))))))
  (check-match (force (force (force (force (force (force (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5))))))))))))))))))))) (cons 5 x) (promise? x))
  (check-pred promise? (force (cdr (force (force (force (force (force (force (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5))))))))))))))))))))))))

  (define-relation (l n)
    (m n))
  (define-relation (m n)
    (mplus (l n) (unit n)))

  (check-pred promise? (force (l 5)))
  (check-pred promise? (force (force (l 5))))
  (check-match (force (force (force (l 5)))) (cons 5 x) (promise? x))
  (check-pred promise? (force (cdr (force (force (force (l 5)))))))
  (check-match (force (force (cdr (force (force (force (l 5))))))) (cons 5 x) (promise? x))
  (check-pred promise? (cdr (force (force (cdr (force (force (force (l 5)))))))))
  (check-pred promise? (cdr (force (force (cdr (force (force (force (l 5)))))))))
  (check-pred promise? (force (cdr (force (force (cdr (force (force (force (l 5))))))))))
  (check-match (force (force (cdr (force (force (cdr (force (force (force (l 5)))))))))) (cons 5 x) (promise? x))
  (check-pred promise? (cdr (force (force (cdr (force (force (cdr (force (force (force (l 5))))))))))))
  (check-pred promise? (force (cdr (force (force (cdr (force (force (cdr (force (force (force (l 5)))))))))))))
  (check-pred promise? (force (cdr (force (force (cdr (force (force (cdr (force (force (force (l 5)))))))))))))
  (check-pred promise? (cdr (force (force (cdr (force (force (cdr (force (force (force (l 5))))))))))))
  (check-match (force (force (cdr (force (force (cdr (force (force (force (l 5)))))))))) (cons 5 x) (promise? x))
  )

(test-begin 
  (define-relation (e n)
    (f n))
  (define-relation (f n)
    (g n))
  (define-relation (g n)
    (e n))

  (check-pred promise? (e 5))
  (check-pred promise? (force (e 5)))
  (check-pred promise? (force (force (force (e 5)))))
  (check-pred promise? (force (force (force (force (force (e 5)))))))
  (check-pred promise? (force (force (force (force (force (force (force (e 5)))))))))
  )


