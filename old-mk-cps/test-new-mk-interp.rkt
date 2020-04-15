#lang racket

(define (ee g)
  (match g
    [`s (λ (sk) (λ (fk) ((sk #t) fk)))]
    [`f (λ (sk) (λ (fk) (fk)))]
    [`(c g1 g2) (λ (sk) (λ (fk) ((ee g) (λ (s) (λ (fk) )))))]
    [`(d g1 g2) ]
    [`(r) ]))

;; Danvy et al., in "A Unifying Approach to Goal-Directed Evaluation",
;; compare the following approaches, and they're able to relate
;; between them.

;; Unlike Danvy et al., lazy, odd-initial ('odd' in the terminology of
;; Wadler "... Without Even Being Odd") streams themselves are
;; possible returned values. This change impacts both the datatype
;; that we can possibly return, as well as the structure of the
;; continuations.

;; Building an append-map may not be the smartest way to do this:
;; Instead, we should build a map, and a join (that works like flatten).

;; (See the deriving one version from the other rules of monads. --
;; Dave Herman)

;; Assuming we *have* a monad, we should derive the alternate definition. 




We sometimes do a stream, we sometimes do a list.

So more complicated datatype, more complicated defunctionalization. 

That should lead us to ... something? 

;; We have the sk/fk model.
;; We have that that's related to the stream based approach by Church encoding

;; We have the continuation model
;; We have that related to the lists model? 
