#lang racket
(require racket/trace)
;; use macros to define the program
;; How to refunctionalize the defunctionalization of the thing? 

(struct later (value) #:transparent)
(struct now (value) #:transparent)

(define (d-inner-k $₂^ k^) `(d-inner-k ,$₂^ ,k^))
(define (d-outer-k g₁^ k^) `(d-outer-k ,g₁^ ,k^))
(define (c-k g₂^ k^) `(c-k ,g₂^ ,k^))
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
(define (appo-th) `(appo-th))
(define (unprodo-th) `(unprodo-th))
(define (alwayso-th) `(alwayso-th))

(define (apply-k k $)
  (match k
;;    [`($app-map-cons-inner-k ,$₂^ ,k^) ($append $ $₂^ k^)]
;;    [`($app-map-cons-outer-k ,g^ ,k^)  (ee g^ (d-inner-k $ k^))]
;;    [`($app-map-later ,g^ ,k^)         ($append-map g^ $ k^)]
;;    [`(mat-len-cons-k ,a^ ,k^)            (apply-k k^ (now (cons a^ $)))]
;;    [`(inner-init-k n^ k^)             (mature-to-length n^ $ k^)]
;;    [`(outer-init-k n^ k^)             (pull $ k^)]
    [`(empty-k) $]
    [`(d-inner-k ,$₂^ ,k^)             ($append $ $₂^ k^)]
    [`(d-outer-k ,g^ ,k^)              (ee g^ (d-inner-k $ k^))]
    [`(c-k ,g^ ,k^)                    ($append-map g^ $ k^)]
;; This one is the interleave.
    [`($app-later-k ,$₂^ ,k^)          ($append $₂^ $ k^)]
    [`($app-cons-k ,a^ ,k^)            (apply-k k^ (now (cons a^ $)))]
    [`($app-map-th ,g^ ,k^)            (apply-k k^ (c-k g^ $))]
    [`($app-th ,$₂^ ,k^)               (apply-k k^ ($app-later-k $₂^ $))]
    [`(mat-len-later-k ,n^ ,k^)        (mature-to-length n^ $ k^)]
    [`(pull-k ,k^)                     (pull $ k^)]
    [`(appo-th)                        (ee `(d (c s s) (c s (appo))) $)]
    [`(unprodo-th)                     (ee `(unprodo) $)]
    [`(alwayso-th)                     (ee `(d s (alwayso)) $)]))

(define (ee g k)
  (match g
    [`(d ,g₁ ,g₂)  (ee g₂ (d-outer-k g₁ k))]
    [`(c ,g₁ ,g₂)  (ee g₁ (c-k g₂ k))]
    [`s            (apply-k k (now (cons '#t (now '()))))]
    [`f            (apply-k k (now '()))]
    [`(appo)       (apply-k k (later (appo-th)))]
    [`(unprodo)    (apply-k k (later (unprodo-th)))]
    [`(alwayso)    (apply-k k (later (alwayso-th)))]))

(define ($append-map g $ k)
  (match $
    [(now c)
     (match c
       ['()         (apply-k k (now c))]
       [(cons a $d) ($append-map g $d (d-outer-k g k))])]
    [(later c)      (apply-k k (later ($app-map-th g c)))]))

(define ($append $ $₂ k)
  (match $
    [(now c) 
     (match c
       ['()         (apply-k k $₂)]
       [(cons a $d) ($append $d $₂ ($app-cons-k a k))])]
    [(later c)      (apply-k k (later ($app-th $₂ c)))]))

(define (pull $ k)
  (match $
    [(now c)   (apply-k k (now c))]
    [(later c) (apply-k c (pull-k k))]))

(define (mature-to-length n $ k)
  (cond
    [(zero? n)         (apply-k k $)]
    [else 
     (match $
       [(now c)
        (match c
          ['()         (apply-k k (now '()))]
          [(cons a $d) (mature-to-length (sub1 n) $d ($app-cons-k a k))])]
       [(later c)      (apply-k c (mat-len-later-k n k))])]))

;; (define (mature $)
;;   (match $
;;     [`(now ,$)
;;      (match $
;;        ['() (now '())]
;;        [(cons a $) (now (cons a (mature (force $))))])]
;;     [`(later ,$) (mature (force $))]))

(ee '(appo) (pull-k (mat-len-later-k 3 (empty-k))))


