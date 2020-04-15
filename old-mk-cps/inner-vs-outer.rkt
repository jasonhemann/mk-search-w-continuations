#lang racket

;; Dan
;; How general is this optimization?

(define (ep q n k)
  (ee q
    (λ ($)
      (pull $ 
        (λ (m$)
          (take n m$ k))))))

;; So we make the continuations RI.  So far, nothing out of the
;; ordinary.

(define (ep-inner-k n k)
  (λ (m$)
    (take n m$ k)))

(define (ep-outer-k n k)
  (λ ($)
    (pull $ (ep-inner-k n k))))

(define (ep q n k)
  (ee q (ep-outer-k n k)))

;; Notice here that the outer-k takes n as an argument just so that it
;; can build the inner k. We could just as well pass along the
;; pre-built inner-k to start with.

(define (ep-inner-k n k)
  (λ (m$)
    (take n m$ k)))

(define (ep-outer-k n k)
  (λ ($)
    (pull $ k)))

(define (ep q n k)
  (ee q (ep-outer-k n (ep-inner-k n k))))

;; Which means that the outer-k constructor no longer needs n at all.

(define (ep-inner-k n k)
  (λ (m$)
    (take n m$ k)))

(define (ep-outer-k k)
  (λ ($)
    (pull $ k)))

(define (ep q n k)
  (ee q (ep-outer-k (ep-inner-k n k))))

;; Is this wrong? Is this forbidden for some reason?  If they're just
;; constructors, and constructors are simple, then shouldn't I be able
;; to nest constructions like this?

;; If this is wrong, why? If this isn't wrong, is it actually an
;; optimization? And if it's an optimization, in what situations is it
;; an improvement, and how general is this technique?

;; Is this one of those advanced continuation optimizations you talked
;; about in EOPL(1?)
