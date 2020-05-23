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

(trace-define kons (λ (a) (trace-lambda #:name kons-inner (fk) (cons a (fk)))))
(trace-define nill (λ () '()))
;; It's not clear what this should do.
;; We could implement a run* behavior.
;; We could also implement some run $n behavior
;; Where we could be tracking the actual number of answers
;; We could be tracking the number of pulls
;; We could be tracking just /whether/ or not it comes back with an answer
;; ((((mzero) loop) kons) nill)
;; ((((unit 5) loop) kons) nill)

;; Something about this doesn't work because as we come through, we
;; lose an fk, or the sk. It's complex and complicated. 


;; I construct the (d 5)
;; That's a monadic computation
;; So we have to pass it
;; init dk (loop)
;; init sk (kons)
;; init fk (nill)

;; That computation should then, AFAIK, invoke the dk
;; What should then happen once we invoke the dk. Since it's delayed?
;; At least in a run*, it should continue that computation
;; the delay should continue the computation
;; What should it mean for the delay to continue this computation?

;; Ultimately I think it should mean that we call g again with _a_ dk
;; and also that same sk and fk.



;; Think more about what should happen w/two answers. 

;; (define loop
;;   (λ (c)
;;     (pretty-print "eating the sk and fk now")
;;     (λ (sk)
;;       (λ (fk)
;;         (pretty-print `(((,c ,loop) ,sk) ,fk))
;;         (let ((f (c loop)))
;;           (pretty-print "computed (c loop)")
;;           (let ((g (f sk)))
;;             (pretty-print "computed ((c loop) sk)")
;;             (let ((h (g fk)))
;;               (pretty-print "computed (((c loop) sk) fk)")
;;               h)))))))


(define loop
  (λ (c)
    (λ (sk)
      (λ (fk)
        (((c loop) sk) fk)))))

;; This is somehow returning the empty failure continuation...
;; (((((d 5) loop) kons) nill))

((((unit 5) loop) kons) nill)

(define-relation (a n)
  (b n))
(define-relation (b n)
  (c n))
(define-relation (c n)
  (d n))
(define-relation (d n)
  (unit n))

;; ((((d 5) loop) kons) nill)

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

((((d 5) loop) kons) nill)
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


