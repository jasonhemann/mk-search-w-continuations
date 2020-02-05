#lang racket

;; We still fix the sets of relations of the programming language.

;; We construct the heuristic interleaving stream-monadic version of
;; this interpreter. In this monadic interpreter, the language's
;; implementation is almost literally the same as the monadic
;; interpretation; there aren't any complex monadic operations in the
;; miniKanren language, the way there were in Danvy's Icon language.

;; We also have not yet addressed the critical difference in our
;; implementations: the delay and interleave behavior. This is bald
;; and exposed here still, in this quasi-monadic implementation. 

(define (eval g s)
  (match g
    [`(succeed) (unit s)]
    [`(fail) (mzero)]
    [`(^ ,g1 ,g2) (bind (eval g1 s) (λ (s) (eval g2 s)))] ;; (λ (s) (eval g2 s)) or g2
    [`(v ,g1 ,g2) (mplus (eval g1 s) (eval g2 s))]
    [`(threeo) (λ () (eval '(twoo) s))]
    [`(twoo)   (λ () (eval '(one) s))]
    [`(oneo)   (λ () (eval '(succeed) s))]
    [`(alwayso) (λ () (eval '(v (succeed) (alwayso)) s))]))

(define ($append $1 $2)
  (cond
    [(null? $1) $2]
    [(procedure? $1) (λ () ($append $2 ($1)))]
    [(pair? $1) (cons (car $1) ($append (cdr $1) $2))]))

(define ($append-map $1 f) ;; rename to g, if it's a goal
  (cond
    [(null? $1) '()]
    [(procedure? $1) (λ () ($append-map $1 f))] 
    [(pair? $1) ($append (f (car $1)) ($append-map (cdr $1) f))])) ;; (f (car $1)) or (eval (car $1) f)

(define (unit s)
  (list s))

(define (mzero)
  (list))

(define (bind $ f)
  ($append-map $ f))

(define (mplus $1 $2)
  ($append $1 $2))

;; We also do not have a `run` for the monad's implementation.

;; If we in fact do have a monad here, it should be the composition of
;; at least one Monad transformer over an identity monad. Possibly
;; also the composition of some other more interesting base monad and
;; transformer.
