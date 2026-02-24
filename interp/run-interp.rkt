#lang racket

;; Struct Defns k := <Defn k>_1 + ... + <Defn k>_k 
;; Computations <Struct Defns k> := (disj C C) | Symbol | (rel# k)   

;; 1. Doubly CPS
;; 2. Racket Delimited control

;; valof-cps l : Struct k -> C k -> Maybe? Partial? Listof Symbol

(define (valof-cps list-of-bods e)
  (match e
    [`(run ,c)
     ;; needs to know how to receive a delay and "do something with it".
     (valof^-cps c list-of-bods ?k ?k2)]))


(define (valof^-cps e list-of-bods ?k ?k2)
  (match e
    [`,e #:when (symbol? e)
         ;; e, and "then more"
         
         ]
    [`,e #:when (and (natural? e) (< e (length list-of-bods)))
         ;; return a delay of (index-of list-of-bods e)
         ] 
    [`(disj ,c1 ,c2)
         ;; how to receive a delay, and how to return a delay
     ]))
