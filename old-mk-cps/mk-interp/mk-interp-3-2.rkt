#lang racket
;;(require racket/trace)
;; use macros to define the program
;; How to refunctionalize the defunctionalization of the thing? 

(struct later (value) #:transparent)
(struct now (value) #:transparent)

(define (apply-k k $)
  (match $
    [else (k $)]))

(define (disj-inner-k $₂^ k^)
  (λ ($)
    ($append $ $₂^ k^)))

(define (disj-outer-k g₁^ k^)
  (λ ($)
    (ee g₁^ (disj-inner-k $ k^))))

(define (conj-k g₂^ k^)
  (λ ($)
    ($append-map g₂^ $ k^)))

(define ($app-map-cons-inner-k $₂^ k^) 
  (λ ($) 
    ($append $ $₂^ k^)))

(define ($app-map-cons-outer-k g^ k^)
  (λ ($) 
    (ee g^ ($app-map-cons-inner-k $ k^))))

(define ($app-map-later g^ k^)
  (λ ($)
    ($append-map g^ $ k^)))

(define ($app-cons-k a^ k^)
  (λ ($)
    (apply-k k^ (now (cons a^ $)))))

(define ($app-later-k $₂^ k^)
  (λ ($)
    ($append $₂^ $ k^)))

(define (mat-len-cons-k a^ k^)
  (λ ($)
    (apply-k k^ (now (cons a^ $)))))

(define (mat-len-later-k n^ k^)
  (λ ($)
    (mature-to-length n^ $ k^)))

(define (pull-k k)
  (λ ($) 
    (pull $ k)))

(define (empty-k)
  (λ ($)
    $))

(define (inner-init-k k)
  (λ ($)
    (mature-to-length 3 $ k)))

(define (outer-init-k k)
  (λ ($)
    (pull $ k)))

(define (ee g k)
  (match g
    [`(conj ,g₁ ,g₂) (ee g₁ (conj-k g₂ k))]
    [`(disj ,g₁ ,g₂) (ee g₂ (disj-outer-k g₁ k))]
    [`succeed (apply-k k (now (cons '#t (now '()))))]
    [`fail (apply-k k (now '()))]
    [`(appendo) 
     (apply-k k (later (λ (k) 
                         (ee `(disj
                               (conj succeed succeed)
                               (conj succeed (appendo)))
                             k))))]
    [`(unproductiveo) 
     (apply-k k (later (λ (k) 
                         (ee `(unproductiveo) k))))]
    [`(alwayso) 
     (apply-k k (later (λ (k) 
                         (ee `(disj
                               succeed
                               (alwayso))
                             k))))]))

(define ($append-map g $ k)
  (match $
    [(now c)
     (match c
       ['() (apply-k k (now c))]
       [(cons a $d) ($append-map g $d ($app-map-cons-outer-k g k))])]
    [(later c)
     (apply-k k (later (λ (k) 
                         (force c ($app-map-later g k)))))]))

(define ($append $ $₂ k)
  (match $
    [(now c) 
     (match c
       ['() (apply-k k $₂)]
       [(cons a $d) ($append $d $₂ ($app-cons-k a k))])]
    [(later c)
     (apply-k k (later (λ (k)
                         (force c ($app-later-k $₂ k)))))]))

;; this is tricky here, and it's *because* we're running a
;; computation.

;; Because it is a continuation. Because it is a captured continuation
;; we're putting into place

(define (force c k)
  (c k)) 

(define (pull $ k)
  (match $
    [(now c) (apply-k k (now c))]
    [(later c) (force c (pull-k k))]))

(define (mature-to-length n $ k)
  (cond
    [(zero? n) (apply-k k $)]
    [else 
     (match $
       [(now c)
        (match c
          ['() (apply-k k (now '()))]
          [(cons a $d) (mature-to-length (sub1 n) $d (mat-len-cons-k a k))])]
       [(later c) (force c (mat-len-later-k n k))])]))

;; (define (mature $)
;;   (match $
;;     [`(now ,$)
;;      (match $
;;        ['() (now '())]
;;        [(cons a $) (now (cons a (mature (force $))))])]
;;     [`(later ,$) (mature (force $))]))

(ee '(appendo) (outer-init-k (inner-init-k (empty-k))))
