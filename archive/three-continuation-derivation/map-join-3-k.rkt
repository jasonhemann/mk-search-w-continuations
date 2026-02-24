#lang racket
(require racket/trace)
;; It seems like Sk has to take a dk as an argument.
;; Because in join 

(trace-define  (unit a)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        ((sk a)
         fk)))))

;; I could invoke an error k
(trace-define  (fail)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (fk)))))

;; We could do one of a couple of things.
;; We could treat it as a type of success.
;; We could treat it as its own thing.
;; We cannot treat it as failure, because failure is unconditional.
;; But failure musn't be unconditional.

;; If failure is unconditional, then we must have captured all the
;; waiting and remaining context and control operations there, and it would have to be able to take and return a continuation. 

(trace-define  (unprod n)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (dk (unprod (add1 n))) ;; (unprod ...) is a c
        ))))

(trace-define  (nats n)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (dk (disj (unit n) (nats (add1 n)))) ;; (disj ...) is a c
        ))))

;; And map takes an f :: a -> a, and passes the results to sk, with the option to bail
(trace-define  (map f c)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (((c (λ (b)
               (λ (fk)
                 ((sk (f b))
                  fk))))
          dk)
         fk)))))

;; *because* this is an mm, the results are 'm's, and so we can just
;; run them without an f.
(trace-define  (join mm)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (((mm (λ (mb) ;; we don't want to say that our delay is a success,
                      ;; b/c then we can't know what to do
                (((mb sk)
                  dk) ;; has to be here, because mb is a computation
                 fk)))
          dk)
         fk)))))

(trace-define  (disj c c2)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (((c sk)
           dk) 
         (λ ()
           (((c2 sk)
             dk) 
            fk)))))))

(trace-define  (bind c f) (join (map f c)))

(trace-define kons (curry (λ (a) (λ (c) (cons a (c))))))
(trace-define nill (thunk '()))

((((unit 5) kons) identity) nill)
((((bind (unit 5) (λ (x) (unit (add1 x)))) kons) identity) nill)
((((disj (unit 5) (unit 6)) kons) identity) nill)
((((disj (disj (unit 4) (unit 5)) (unit 6)) kons) identity) nill)
((((disj (unit 4) (disj (unit 5) (unit 6))) kons) identity) nill)
((((bind (disj (unit 6) (unit 5)) (λ (x) (unit (add1 x)))) kons) identity) nill)
((((bind (disj (unit 5) (unit 6)) (λ (x) (unit (add1 x)))) kons) identity) nill)
((((bind (unit 5) (λ (x) (bind (unit (add1 x)) (λ (x) (unit (add1 x)))))) kons) identity) nill)


