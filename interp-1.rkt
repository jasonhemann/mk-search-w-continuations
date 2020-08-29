#lang racket
(require rackunit)

;; An implementation of the continuation semantics from Fig. 5 of
;; Danvy et al "Unified Approach to Goal-Oriented Languages"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Danvy gives us in Fig 4 how to create lists and append them, using
;; that o operator ("compose").

;; The initial continuation here is the `cons`, curried
(define mtl '())
(define ccons (curry cons)) ;; because we don't have built-in currying like semantics do.

;; For a given Hughes list l, given a definitions of cons and the empty list
;; We can get the standard representation of l
(define as-list (lambda (l) ((l ccons) mtl)))

;; (k → (l → l)) × (k → (l → l)) → (k → (l → l))
(define (app xs ys)
  (λ (k) (compose (xs k) (ys k))))
;; Hughes' point was that we get constant-time append.

;; (λ (k) (λ (l) ((k 5) ((k 6) l)))) 
;; is unsurprisingly a two-element prefix


;; The initial continuation is Ccons. The real cons.
;; Maybe intitial continuation is encoding, Cappend?
(test-begin
  (define nilc (λ (k) (λ (l) l)))
  (define (kons a) (λ (c) (λ (ys) ((c a) ys))))
  ;; the same, but eta reduced.
  (define (unitc a) (λ (k) (k a)))

  (check-equal? (as-list (kons 5)) '(5))
  (check-equal? (as-list nilc) '())
  (check-equal? (as-list (app nilc nilc)) '())
  (check-equal? (as-list (app nilc (unitc 5))) '(5))
  (check-equal? (as-list (app (unitc 5) nilc)) '(5))
  (check-equal? (as-list (app (unitc 5) (unitc 6))) '(5 6))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An implementation of the continuation semantics from Fig. 5 of
;; Danvy et al "Unified Approach to Goal-Oriented Languages"

(define evalₖ
  (match-lambda
    [n #:when (number? n) (λ (k) (k n))]
    [`(,e₁ to ,e₂)
     (λ (k)
       ((evalₖ e₁)
        (λ (i)
          ((evalₖ e₂)
           (λ (j)
             ((toₖ i j) k))))))]
    ;; NB. Not in tail form in finality
    [`(,e₁ + ,e₂)
     (λ (k)
       ((evalₖ e₁)
        (λ (i)
          ((evalₖ e₂)
           (λ (j) (k (+ i j)))))))]
    [`(,e₁ <= ,e₂)
     (λ (k)
       ((evalₖ e₁)
        (λ (i)
          ((evalₖ e₂)
           (λ (j)
             ((leqₖ i j) k))))))]
    [`(if ,e₀ then ,e₁ else ,e₂)
     (λ (k)
       (λ (l)
         ((evalₖ e₀)
          (λ (_)   ;; Ignore the value of the test
            (λ (_) ;; Ignore the value of the other branch
              ((evalₖ e₁) k l)))
          ;; Why is this not eagerly evaluating the else branch?
          ((evalₖ e₂) k l))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This *is* almost a continuation. Tail position and all. We just
;; lifted the lambda out to a global function I think so as to make
;; the program easier to read. Assuming the alternate is an "mzero"ish
;; element. 

(define (leqₖ i j)
  (λ (k)
   (if (<= i j) (k j) (λ (l) l))))

;; Several things bear note.
;; First this is not a lifted continuation, per se. 
;; toₖ is itself recursive. Continuations are not recursive.

;; Secondly, this isn't (at least obviously) in tail form.
;; (k i) and ((toₖ (add1 i) j) k) both seem like serious calls.

;; At the very least, one would have to argue why they are not.
;; General recursion would eat arbitrary stack frames. When we
;; have i << j, that should blow the stack. 

;; This method makes sense if you think in terms of concrete
;; representations. If this is a function for building up
;; parameterized list prefixes (that is, when you provide
;; the tail, we return to you the full list) and that
;; function is then curried and parameterized by its cons
;; then this makes sense to read

;; Read the `else` case of this if block as "compose
;; prefix-maker i with prefix-maker j". That's Hughes-y

(define (toₖ i j)
  (λ (k)
    (if (> i j)
        (λ (l) l)
        (compose (k i) ((toₖ (add1 i) j) k)))))

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
;; Several things bear note(check-equal? (as-list (evalₖ '(4 to (5 to 7)))) '(4 5 4 5 6 4 5 6 7))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; So, in comes this ifte component.

;;  ((evalₖ e₀)
;;   (λ (_)
;;     (λ (_)
;;       ((evalₖ e₁) k l)))
;;   ((evalₖ e₂) k l))

;; How does this make sense?
;; Well, how does this build a prefix?

;; Here, now, by analogy with church encoding.
;; c0 = (lambda (s) (lambda (z) z))
;; c1 = (lambda (s) (lambda (z) (s z)))

;; 0 is "false" and any other value is "truthy" (in the biz)
;; Every cn except c0 uses the s method.
;; c0 is the only one that does not.

;; So if evalₖ e₀ produces an empty list, likewise, we don't use the
;; first element, instead returning the second.

;; In every other instance we apply (lambda (_) (lambda (_) ...) to
;; the first element of the list, and then pass that the remaining
;; list argument, I think.
