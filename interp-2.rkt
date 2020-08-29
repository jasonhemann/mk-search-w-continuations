#lang racket
(require rackunit)
(require "monads.rkt")

;; Attempted monadic implementation of the Fig. 5 continuation
;; semantics from Danvy et al's "Unified Approach to Goal-Oriented
;; Languages"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The initial continuation here is the `cons`, curried
(define mtl '())
(define ccons (curry cons)) ;; because we don't have built-in currying like semantics do.

;; This should be essentially a "run"
(define as-list (lambda (l) ((l ccons) mtl)))

;; A 0-element to the monoid? Maybe? 
(define (mz)
  (lambda (l)
    l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Attempted monadic implementation of the Fig. 5 continuation
;; semantics from Danvy et al's "Unified Approach to Goal-Oriented
;; Languages"

;; In all the places we successfully `return`, we have η-reduced away
;; the (λ (l) (E l)). Not sure what to do 

(define evalₖ
  (match-lambda
    [n #:when (number? n)
       (return n)]
    [`(,e₁ to ,e₂) 
     (doₖ 
      (i <- (evalₖ e₁))
      (j <- (evalₖ e₂))
      (toₖ i j))]
    ;; NB. Not in tail form in finality
    [`(,e₁ + ,e₂)
     (doₖ
      (i <- (evalₖ e₁))
      (j <- (evalₖ e₂))
      (return (+ i j)))]
    [`(,e₁ <= ,e₂)
     (doₖ
      (i <- (evalₖ e₁))
      (j <- (evalₖ e₂))
      ;; in-lining (leqₖ i j)
      (if (<= i j)
          (return j)
          ;; dunno about the consequent
          (λ (l) l)))]
    [`(if ,e₀ then ,e₁ else ,e₂)
     ;; Don't know what to do w/this either.
     ;; Must be this is C2 b/c we cannot η away the (λ (l) ...)
     (λ (k)
       (λ (l)
         ((evalₖ e₀)
          (λ (_)   ;; Ignore the value of the test
            (λ (_) ;; Ignore the value of the other branch
              ((evalₖ e₁) k l)))
          ((evalₖ e₂) k l))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; We in-lined the above usage. To reiterate, the only interesting
;; question remaining is treating \l.l
(define (leqₖ i j)
  (λ (k)
   (if (<= i j) (k j) (λ (l) l))))

;; This one still has lots to recommend itself as interesting.
;; I don't know what to say about it.
(define (toₖ i j)
  (λ (k)
    (if (> i j)
        (λ (l) l)
        (compose (k i) ((toₖ (add1 i) j) k)))))

;; Something like app looks at least bind-ish. 
;; It at least *kinda* has the right shape. But no. Icky.
(define (app xs ys)
  (λ (k) (compose (xs k) (ys k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal? (as-list (evalₖ '4)) '(4))
(check-equal? (as-list (evalₖ '(5 + 7))) '(12))
(check-equal? (as-list (evalₖ '(5 <= 4))) '())
(check-equal? (as-list (evalₖ '(5 <= 5))) '(5))
;; list of the bs for which (a ∈ A, b ∈ B) a <= b 
(check-equal? (as-list (evalₖ '(5 <= 7))) '(7))
(check-equal? (as-list (evalₖ '(5 to 4))) '())
(check-equal? (as-list (evalₖ '(5 to 5))) '(5))
(check-equal? (as-list (evalₖ '(5 to 6))) '(5 6))
(check-equal? (as-list (evalₖ '(5 to 7))) '(5 6 7))
;;(check-equal? (as-list (evalₖ '(4 to (5 to 7)))) '(4 5 4 5 6 4 5 6 7))
