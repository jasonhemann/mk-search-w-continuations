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

|#

;; Suspicious that my implementations maybe don't exactly describe
;; the mK search as I want to here. 

;; It's not fully clear what the loop should do.
;; IMO this gives a run* behavior.
;; (define loop*
;;   (λ (c)
;;     (λ (sk)
;;       (λ (fk)
;;         (((c loop*) sk) fk)))))

;; We could also implement some run $n behavior
;; Where we track the number of pulls
;; (define (loop n)
;;   (λ (c)
;;     (λ (sk)
;;       (λ (fk)
;;         (if (zero? n)
;;             '()
;;             (((c (loop (sub1 n))) sk) fk))))))

;; Alt definition where we re-start with the initial sk and fk?

;; We could track just /whether/ or not it comes back with _an_ answer.
;; Could we be easily track the no. of answers? I don't know how.
(test-begin
 (check-equal? '() ((((mzero) loop*) kons) nill))
 (check-equal? '(5) ((((unit 5) loop*) kons) nill))

 (define-relation (a n)
   (b n))
 (define-relation (b n)
   (c n))
 (define-relation (c n)
   (d n))
 (define-relation (d n)
   (unit n))

 (check-equal? '(5) (run (d 5)))
 (check-equal? '(5) ((((a 5) loop*) kons) nill))

 (define-relation (e n)
   (f n))
 (define-relation (f n)
   (g n))
 (define-relation (g n)
   (e n))

 (check-equal? '(5) (run 500 (mplus (e 5) (a 5))))

 (define-relation (h n)
   (i n))
 (define-relation (i n)
   (j n))
 (define-relation (j n)
   (mplus (h n) (a n)))

 ;; Think more about what should happen w/two answers. 

 (define-relation (m n)
   (mplus (unit n) (m n)))

 ;; This seems promising, don't know if it's right.
 (check-equal? 49 (length ((((mplus (m 5) (m 6)) (loop 50)) kons) nill)))
 (check-equal? '(5 6 5 6) ((((mplus (m 5) (m 6)) (loop 5)) kons) nill))

 (define-relation (l n)
   (mplus (unit n) (mplus (unit n) (l n))))

 ;; This indicates that we are measuring the number of pulls, not the number of answers.
 (check-equal? 98 (length ((((mplus (l 5) (l 6)) (loop 50)) kons) nill)))

 (define-relation (unproductiveo x)
   (unproductiveo x))

 (check-equal? '() ((((unproductiveo 5) (loop 50)) kons) nill))
 )
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


((((mplus
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
      (d 1)))) loop*)
  kons)
 nill)
