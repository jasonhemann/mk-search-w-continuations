#lang racket

;; (struct 𝕀 ()) ;; fake a unit constructor. Like, real Haskell unit.

;; (struct later (value) #:transparent)
;; (struct end   () #:transparent)
;; (struct more  (a d) #:transparent)

;; JStream a = End | More a (JStream a) | Later of (𝕀 → JStream a)

;; By the way: how should we think about this datatype? Should this be
;; correct? Are constructors eager or lazy? That is, should Later need
;; to take (𝕀 → JStream a), or could it just take JStream a? 

;; We have a more complicated datatype than any of "eager list" "lazy
;; stream" "odd stream" "even stream". We should imagine a more
;; complicated defunctionalization.

;; By Church encoding, Danvy et al relate the sk/fk model with the
;; stream based approach. Likewise by Church encoding, Danvy et al
;; relate the continuation model with the the lists model of
;; backtracking (eager).

;; We try and follow that pattern and Church encode, and see if that
;; scales.

;; JStream a = End | More a (JStream a) | Later of (𝕀 → JStream a)

;; Church encoding of the datatype:

(define (end)
  (λ (sm)
    (λ (sl)
      (λ (se) ;; because it's an fk, and that's the way they do it in Danvy
        (se)))))

(define (more x xs)
  (λ (sm)
    (λ (sl)
      (λ (se)
        (sm x xs)))))

(define (later th) ;; Still questionable
  (λ (sm)
    (λ (sl)
      (λ (se)
        sl))))




;; One open question is: should we *need* three continuations, for the
;; three elements of our datatype? Or should we be able to encode the
;; three elements somehow into two. Two continuations are so often
;; sufficient. It would be interesting to know that we *need* three
;; continuations, and that they fall out by Church encoding in the
;; straightforward way. It would also be interesting to show that two
;; continuations suffice, and to see how to encode the three
;; continuations model with merely two. Might this suggest an
;; analogous simplification to the streams model?

;; Is it the case that, in our model, we "reified the continuation"?
;; By making a stream a "value we return", is it the case that in the
;; analogous continuation operation, we make the continuation a kind
;; of returnable value? And how does that compare to (k^ k) in let/cc
;; and throw?

