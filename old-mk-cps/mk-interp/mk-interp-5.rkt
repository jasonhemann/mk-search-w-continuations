#lang racket
(require racket/trace)
;; use macros to define the program
;; How to refunctionalize the defunctionalization of the thing? 

(struct later (value) #:transparent)
(struct now (value) #:transparent)

(define (disj-inner-k $₂^ k^) `(disj-inner-k ,$₂^ ,k^))
(define (disj-outer-k g₁^ k^) `(disj-outer-k ,g₁^ ,k^))
(define (conj-k g₂^ k^) `(conj-k ,g₂^ ,k^))
(define ($app-map-cons-inner-k $₂^ k^) `($app-map-cons-inner-k ,$₂^ ,k^))
(define ($app-map-cons-outer-k g^ k^) `($app-map-cons-outer-k ,g^ ,k^))
(define ($app-map-later g^ k^) `($app-map-later ,g^ ,k^))
(define ($app-cons-k a^ k^) `($app-cons-k ,a^ ,k^))
(define ($app-later-k $₂^ k^) `($app-later-k ,$₂^ ,k^))
(define (mat-len-cons-k a^ k^) `(mat-len-cons-k ,a^ ,k^))
(define (mat-len-later-k n^ k^) `(mat-len-later-k ,n^ ,k^))
(define (pull-k k^) `(pull-k ,k^))
(define ($app-map-th g^ k^) `($app-map-th ,g^ ,k^))
(define ($app-th $₂^ k^) `($app-th ,$₂^ ,k^))
(define (empty-k) `(empty-k))
(define (inner-init-k) `(inner-init-k))
(define (outer-init-k) `(outer-init-k))
(define (appendo-th) `(appendo-th))
(define (unproductiveo-th) `(unproductiveo-th))
(define (alwayso-th) `(alwayso-th))

(define (apply-k k $)
  (match k
    [`(empty-k) $]
    [`(disj-inner-k ,$₂^ ,k^)          ($append $ $₂^ k^)]
    [`($app-map-cons-inner-k ,$₂^ ,k^) ($append $ $₂^ k^)]
    [`(disj-outer-k ,g₁^ ,k^)          (ee g₁^ (disj-inner-k $ k^))]
    [`($app-map-cons-outer-k ,g^ ,k^)  (ee g^ ($app-map-cons-inner-k $ k^))]
    [`($app-map-later ,g^ ,k^)         ($append-map g^ $ k^)]
    [`(conj-k ,g₂^ ,k^)                ($append-map g₂^ $ k^)]
    [`($app-later-k ,$₂^ ,k^)          ($append $₂^ $ k^)]
    [`($app-cons-k ,a^ ,k^)            (apply-k k^ (now (cons a^ $)))]
    [`(mat-len-cons-k ,a^ ,k^)         (apply-k k^ (now (cons a^ $)))]
    [`($app-map-th ,g^ ,k^)            (apply-k k^ ($app-map-later g^ $))]
    [`($app-th ,$₂^ ,k^)               (apply-k k^ ($app-later-k $₂^ $))]
    [`(mat-len-later-k ,n^ ,k^)        (mature-to-length n^ $ k^)]
    [`(pull-k ,k^)                     (pull $ k^)]
    [`(outer-init-k)                   (pull $ (inner-init-k))]
    [`(inner-init-k)                   (mature-to-length 3 $ (empty-k))]
    [`(appendo-th) (ee `(disj
                         (conj succeed succeed)
                         (conj succeed (appendo)))
                       $)]
    [`(unproductiveo-th) (ee `(unproductiveo) $)]
    [`(alwayso-th) (ee `(disj
                         succeed
                         (alwayso))
                       $)]))

(define (ee g k)
  (match g
    [`succeed (apply-k k (now (cons '#t (now '()))))]
    [`fail (apply-k k (now '()))]
    [`(disj ,g₁ ,g₂) (ee g₂ (disj-outer-k g₁ k))]
    [`(conj ,g₁ ,g₂) (ee g₁ (conj-k g₂ k))]
    [`(appendo) (apply-k k (later (appendo-th)))]
    [`(unproductiveo) (apply-k k (later (unproductiveo-th)))]
    [`(alwayso) (apply-k k (later (alwayso-th)))]))

(define ($append-map g $ k)
  (match $
    [(now c)
     (match c
       ['() (apply-k k (now c))]
       [(cons a $d) ($append-map g $d ($app-map-cons-outer-k g k))])]
    [(later c) (apply-k k (later ($app-map-th g c)))]))

(define ($append $ $₂ k)
  (match $
    [(now c) 
     (match c
       ['() (apply-k k $₂)]
       [(cons a $d) ($append $d $₂ ($app-cons-k a k))])]
    [(later c) (apply-k k (later ($app-th $₂ c)))]))

;; this is tricky here, and it's *because* we're running a
;; computation.

;; Because it is a continuation. Because it is a captured continuation
;; we're putting into place

;; (define (force c k)
;;   (c k))

(define (pull $ k)
  (match $
    [(now c) (apply-k k (now c))]
    [(later c) (apply-k c (pull-k k))]))

(define (mature-to-length n $ k)
  (cond
    [(zero? n) (apply-k k $)]
    [else 
     (match $
       [(now c)
        (match c
          ['() (apply-k k (now '()))]
          [(cons a $d) (mature-to-length (sub1 n) $d (mat-len-cons-k a k))])]
       [(later c) (apply-k c (mat-len-later-k n k))])]))

;; (define (mature $)
;;   (match $
;;     [`(now ,$)
;;      (match $
;;        ['() (now '())]
;;        [(cons a $) (now (cons a (mature (force $))))])]
;;     [`(later ,$) (mature (force $))]))

(ee '(appendo) (outer-init-k))


