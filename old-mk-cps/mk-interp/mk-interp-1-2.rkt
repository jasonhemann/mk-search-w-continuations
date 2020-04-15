#lang racket

(struct later (value) #:transparent)
(struct now (value) #:transparent)

(define (ee g k)
  (match g
    [`(d ,g1 ,g2) 
     (ee g1 (λ ($)
              ($append
               $ 
               (later 
                (λ (k) 
                  (ee g2 k)))
               k)))]
    [`(c ,g1 ,g2) 
     (ee g1 (λ ($) 
              ($append-map $ g2 k)))]
    [`(s) (apply-k k (now (cons '#t (now '()))))]
    [`(f) (apply-k k (now '()))]
    [`(appo) 
     (apply-k k 
      (later 
       (λ (k) 
         (ee `(d (c (s) (s)) (c (s) (appo))) k))))]
    [`(unpo)
     (apply-k k 
      (later
       (λ (k) 
         (ee '(unpo) k))))]
    [`(listo)
     (apply-k k 
      (later
       (λ (k) 
         (ee '(d (c (s) (s)) (listo)) k))))]))

(define ($append $1 $2 k)
  (match $1
    [(now str)
     (match str
       [`() (apply-k k $2)]
       [`(,s . ,$)
        ($append $ $2 
                 (λ ($) 
                   (apply-k k (now (cons s $)))))])]
    [(later $) 
     (apply-k k
              (later 
               (λ (k) 
                 (apply-k $
                          (λ ($)
                            ($append $ $2 k))))))]))

(define (apply-k $ k)
  (k $))

(define ($append-map $1 g k)
  (match $1
    [(now str)
     (match str
       [`() (apply-k k (now '()))]
       [`(,s . ,$1) 
        (ee g (λ ($)
                ($append $
                         (later 
                          (λ (k)
                            ($append-map $1 g k)))
                         k)))])]
    [(later $)
     (apply-k k (later 
                 (λ (k)
                   (apply-k $ 
                            (λ ($)
                              ($append-map $ g k))))))]))
