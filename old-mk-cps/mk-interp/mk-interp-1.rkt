#lang racket
;;(require racket/trace)
;; use macros to define the program
;; How to refunctionalize the defunctionalization of the thing? 

(struct later (value) #:transparent)
(struct now (value) #:transparent)

(define (ee g)
  (match g
    [`succeed (now (cons '#t (now '())))]
    [`fail (now '())]
    [`(disj ,g1 ,g2) ($append (ee g1) (ee g2))]
    [`(conj ,g1 ,g2) ($append-map g2 (ee g1))]
    [`(appendo) 
     (later (λ () 
             (ee `(disj
                   (conj succeed succeed)
                   (conj succeed (appendo))))))]
    [`(unproductiveo) 
     (later (λ () 
             (ee `(unproductiveo))))]
    [`(alwayso) 
     (later (λ () 
             (ee `(disj
                   succeed
                   (alwayso)))))]))

(define ($append-map g $)
  (match $
    [(now c)
     (match c
       ['() (now c)]
       [(cons a $d) ($append (ee g) ($append-map g $d))])]
    [(later c) (later (λ () ($append-map g (force c))))]))

(define ($append $ $2)
  (match $
    [(now c) 
     (match c
       ['() $2]
       [(cons a $d) (now (cons a ($append $d $2)))])]
    [(later c) (later (λ () ($append $2 (force c))))]))

(define (force $)
  ($))

(define (pull $)
  (match $
    [(now c) (now c)]
    [(later c) (pull (force c))]))

(define (mature-to-length n $)
  (cond
    [(zero? n) $]
    [else 
     (match $
       [(now c)
        (match c
          ['() (now '())]
          [(cons a $d) (now (cons a (mature-to-length (sub1 n) $d)))])]
       [(later c) (mature-to-length n (force c))])]))

;; (define (mature $)
;;   (match $
;;     [`(now ,$)
;;      (match $
;;        ['() (now '())]
;;        [(cons a $) (now (cons a (mature (force $))))])]
;;     [`(later ,$) (mature (force $))]))

(mature-to-length 3 (pull (ee '(appendo))))
