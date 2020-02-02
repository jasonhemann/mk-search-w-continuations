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

;; So. Church encoding a datatype means abstracting over selector
;; functions---the things that help us choose between two pieces of
;; data. Like in if, or, Church cons. But does our Delay/Later
;; structure *does* match up?

;; This took a while to get right. A conceptual link between the list
;; monad and the continuation monad with answer type β List -> β List
;; can be made through Church encoding the higher-order representation
;; of lists proposed by Hughes. So to follow this connection, we have
;; to connect the first-order representation of lists to the
;; higher-order representation, and then between that representation
;; and its Church encoding.

;; So, the Hughes H-O rep has type List a -> List a, for some
;; underlying type a. Well, I see how Nil has that type. Ah. Is "cons
;; x" *itself*, on its own, understood as the description of a list in
;; this Hughes representation? The Hughes implementation being where
;; you just abstract over the base element? The List a datatype has to
;; have a uniform type representation. So, if Nil is List a -> List a,
;; then every other list should also be List a -> List a. "Cons x" is
;; by itself meant as a list representation; there is no "... onto
;; Nil" part.

;; So, it is known that Church encoding a datatype means abstracting
;; over its selector functions. Which are just "cons" in this case.

;; It says that the resulting representation of Lists can be typed as
;; (α -> β -> β) -> β -> β. That is, with answer type (β -> β).

;; But we wanted to make a conceptual connection with a continuation
;; monad with answer type β List -> β List. And that's not what we
;; got. So it seems like there are two uses of β. 

;; He says that Nil and Cons for this list representation "yield"
;; empty_c and unit_c.

;; Nil, under the Church encoding, is λsλys.ys, which is =η to λkλl.l

;; Cons x is λsλys.sc x ys.  He says "Cons" is equivalent to Unit. And
;; Unit x =d λk.k x So both are curried functions. And Cons x has room
;; for an η reduction.

;; We'll do the next obvious thing for the sk/fk semantics and the 
