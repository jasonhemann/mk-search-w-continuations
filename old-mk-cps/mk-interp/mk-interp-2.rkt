#lang racket
;;(require racket/trace)
;; use macros to define the program
;; How to refunctionalize the defunctionalization of the thing? 

(struct later (value) #:transparent)
(struct now (value) #:transparent)

(define (ee g k)
  (match g
    [`succeed (k (now (cons '#t (now '()))))]
    [`fail (k (now '()))]
    [`(disj ,g1 ,g2)
     (ee g2 
         (λ ($₂)
           (ee g1 
               (λ ($)
                 ($append $ $₂ k)))))]
    [`(conj ,g1 ,g2)
     (ee g1 
         (λ ($)
           ($append-map g2 $ k)))]
    [`(appendo) 
     (k (later (λ (k) 
                 (ee `(disj
                       (conj succeed succeed)
                       (conj succeed (appendo)))
                     k))))]
    [`(unproductiveo) 
     (k (later (λ (k) 
                 (ee `(unproductiveo) k))))]
    [`(alwayso) 
     (k (later (λ (k) 
                 (ee `(disj
                       succeed
                       (alwayso))
                     k))))]))

(define ($append-map g $ k)
  (match $
    [(now c)
     (match c
       ['() (k (now c))]
       [(cons a $d)
        ($append-map g $d 
                     (λ ($₂) 
                       (ee g 
                           (λ ($) 
                             ($append $ $₂ k)))))])]
    [(later c)
     (k (later (λ (k) 
                 (force c (λ ($)
                            ($append-map g $ k))))))]))

(define ($append $ $2 k)
  (match $
    [(now c) 
     (match c
       ['() (k $2)]
       [(cons a $d)
        ($append $d $2
                 (λ ($)
                   (k (now (cons a $)))))])]
    [(later c)
     (k (later (λ (k)
                 (force c
                        (λ ($)
                          ($append $2 $ k))))))]))

;; this is tricky here, and it's *because* we're running a
;; computation.

;; Because it is a continuation. Because it is a captured continuation
;; we're putting into place

(define (force c k)
  (c k)) 

(define (pull $ k)
  (match $
    [(now c) (k (now c))]
    [(later c) (force c (λ ($) (pull $ k)))]))

(define (mature-to-length n $ k)
  (cond
    [(zero? n) (k $)]
    [else 
     (match $
       [(now c)
        (match c
          ['() (k (now '()))]
          [(cons a $d)
           (mature-to-length (sub1 n) $d
                             (λ ($)
                               (k (now (cons a $)))))])]
       [(later c)
        (force c
               (λ ($)
                 (mature-to-length n $ k)))])]))

;; (define (mature $)
;;   (match $
;;     [`(now ,$)
;;      (match $
;;        ['() (now '())]
;;        [(cons a $) (now (cons a (mature (force $))))])]
;;     [`(later ,$) (mature (force $))]))

(ee '(appendo)
    (λ ($)
      (pull $
            (λ ($)
              (mature-to-length 3 $
                                (λ ($) $)))))) 
