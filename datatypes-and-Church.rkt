#lang racket

;; We have a more complicated datatype than any of "eager list" "lazy
;; stream" "odd stream" "even stream". We should imagine a more
;; complicated defunctionalization.

;; By Church encoding, Danvy et al relate the sk/fk model with the
;; stream based approach.

;; Likewise by Church encoding, Danvy et al relate the continuation
;; model with the the lists model of backtracking (eager)

;; We should try and follow a pattern and see if that scales.

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
