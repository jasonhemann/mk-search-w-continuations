#lang racket
(require rackunit)
(require racket/trace)
;; (require (submod "mk-streams-derivation-3.rkt" streams-unit-map-join))
;; (require (submod "mk-streams-derivation-3.rkt" streams-bind-return))
(require (submod "mk-streams-derivation-3.rkt" sk/fk-unit-map-join))
;; (require (submod "mk-streams-derivation-3.rkt" sk/fk-bind-return))

#| 

This should be the file in which we /use/ these various
implementations, and to show that they all work the same or
independent of one another, to get the right answers out.

These we should still have the sk and the fk.

If we're in Backtrack a, then the sk should be (λ (a) (λ () _)) 

The second must be a failure continuation, which is nullary. I don't
know the return type, I don't think.

The fk should be (λ () _), with the same return type. 

Hrmm.. so the sk takes an fk which it can invoke for more answers, if
it wants to.

Which we could see in like append or disj. 



We can read them off from their implementations. 

I should be able to do like a manual unrolling of what if we find an answer, 
what if we fail, what if we delay. 

|#

(define kons (λ (a) (λ (fk) (cons a (fk)))))
(define nill (λ () '()))

;; It's not clear what this should do.
;; We could implement a run* behavior.
;; We could also implement some run $n behavior
;; Where we could be tracking the actual number of answers
;; We could be tracking the number of pulls
;; We could be tracking just /whether/ or not it comes back with an answer
(define loop
  (λ (c)
    (λ (sk)
      (λ (fk)
        (((c loop) sk) fk)))))

(check-equal? '() ((((mzero) loop) kons) nill))
(check-equal? '(5) ((((unit 5) loop) kons) nill))

;; Think more about what should happen w/two answers. 



(define-relation (a n)
  (b n))
(define-relation (b n)
  (c n))
(define-relation (c n)
  (d n))
(define-relation (d n)
  (unit n))

(check-equal? '(5) ((((d 5) loop) kons) nill))
(check-equal? '(5) ((((a 5) loop) kons) nill))

;; (check-pred promise? (a 5))
;; (check-pred promise? (force (a 5))) 
;; (check-pred promise? (force (force (a 5)))) 
;; (check-pred promise? (force (force (force (a 5)))))
;; (check-equal? '(5) (force (force (force (force (a 5))))))

(define-relation (h n)
  (i n))
(define-relation (i n)
  (j n))
(define-relation (j n)
  (mplus (h n) (a n)))

;; (check-pred promise? (force (force (force (force (force (force (force (h 5)))))))))
;; (check-pred promise? (force (force (force (force (force (force (force (force (h 5))))))))))
;; (check-pred promise? (force (force (force (force (force (force (force (force (force (h 5)))))))))))
;; (check-pred promise? (force (force (force (force (force (force (force (force (force (force (force (h 5)))))))))))))
;; (check-match (force (force (force (force (force (force (force (force (force (force (force (force (h 5))))))))))))) (cons 5 x) (promise? x))  
;; (check-pred promise? (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5))))))))))))))))
;; (check-pred promise? (force (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5)))))))))))))))))
;; (check-pred promise? (force (force (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5))))))))))))))))))
;; (check-pred promise? (force (force (force (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5)))))))))))))))))))
;; (check-pred promise? (force (force (force (force (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5))))))))))))))))))))
;; (check-pred promise? (force (force (force (force (force (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5)))))))))))))))))))))
;; (check-match (force (force (force (force (force (force (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5))))))))))))))))))))) (cons 5 x) (promise? x))
;; (check-pred promise? (force (cdr (force (force (force (force (force (force (force (cdr (force (force (force (force (force (force (force (force (force (force (force (force (h 5))))))))))))))))))))))))

(define-relation (l n)
  (m n))
(define-relation (m n)
  (mplus (l n) (unit n)))

;; (check-pred promise? (force (l 5)))
;; (check-pred promise? (force (force (l 5))))
;; (check-match (force (force (force (l 5)))) (cons 5 x) (promise? x))
;; (check-pred promise? (force (cdr (force (force (force (l 5)))))))
;; (check-match (force (force (cdr (force (force (force (l 5))))))) (cons 5 x) (promise? x))
;; (check-pred promise? (cdr (force (force (cdr (force (force (force (l 5)))))))))
;; (check-pred promise? (cdr (force (force (cdr (force (force (force (l 5)))))))))
;; (check-pred promise? (force (cdr (force (force (cdr (force (force (force (l 5))))))))))
;; (check-match (force (force (cdr (force (force (cdr (force (force (force (l 5)))))))))) (cons 5 x) (promise? x))
;; (check-pred promise? (cdr (force (force (cdr (force (force (cdr (force (force (force (l 5))))))))))))
;; (check-pred promise? (force (cdr (force (force (cdr (force (force (cdr (force (force (force (l 5)))))))))))))
;; (check-pred promise? (force (cdr (force (force (cdr (force (force (cdr (force (force (force (l 5)))))))))))))
;; (check-pred promise? (cdr (force (force (cdr (force (force (cdr (force (force (force (l 5))))))))))))
;; (check-match (force (force (cdr (force (force (cdr (force (force (force (l 5)))))))))) (cons 5 x) (promise? x))


;; (test-begin 
;;   (define-relation (e n)
;;     (f n))
;;   (define-relation (f n)
;;     (g n))
;;   (define-relation (g n)
;;     (e n))

;;   ;; (check-pred promise? (e 5))
;;   ;; (check-pred promise? (force (e 5)))
;;   ;; (check-pred promise? (force (force (force (e 5)))))
;;   ;; (check-pred promise? (force (force (force (force (force (e 5)))))))
;;   ;; (check-pred promise? (force (force (force (force (force (force (force (e 5)))))))))
;;   )


