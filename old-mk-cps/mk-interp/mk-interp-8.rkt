#lang racket
(require racket/trace)
;; use macros to define the program
;; How to refunctionalize the defunctionalization of the thing? 
(define g 'hukarz)
(define $ 'hukarz)
(define $₂ 'hukarz)
(define k 'hukarz)
(define n 'hukarz)
(define pc 'hukarz)

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
(define (mat-len-later-k k^) `(mat-len-later-k ,k^))
;; (define (pull-k k^) `(pull-k ,k^))
(define ($app-map-th g^ k^) `($app-map-th ,g^ ,k^))
(define ($app-th $₂^ k^) `($app-th ,$₂^ ,k^))
(define (empty-k) `(empty-k))
(define (inner-init-k n k) `(inner-init-k ,n ,k))
(define (outer-init-k k) `(outer-init-k ,k))
(define (appendo-th) `(appendo-th))
(define (unproductiveo-th) `(unproductiveo-th))
(define (alwayso-th) `(alwayso-th))

(trace-define (apply-k) ;; k $
  (match k
;;    [`($app-map-cons-inner-k ,$₂^ ,k^) ($append $ $₂^ k^)]
;;    [`($app-map-cons-outer-k ,g^ ,k^)  (ee g^ (disj-inner-k $ k^))]
;;    [`($app-map-later ,g^ ,k^)         ($append-map g^ $ k^)]
;;    [`(mat-len-cons-k ,a^ ,k^)         (apply-k k^ (now (cons a^ $)))]
    ;; [`(inner-init-k ,n^ ,k^)           (begin
    ;;                                      (set! n n^)
    ;;                                      (set! k k^)
    ;;                                      (set! pc mature-to-length))]
    ;; [`(outer-init-k ,k^)               (begin
    ;;                                      (set! k k^)
    ;;                                      (set! pc pull))]

    [`(empty-k)                        $]
    [`(disj-inner-k ,$₂^ ,k^)
     (begin
       (set! k k^)
       (set! $₂ $₂^)
       (set! pc $append))]
    [`(disj-outer-k ,g^ ,k^)
     (begin
       (set! k (disj-inner-k $ k^))
       (set! g g^) 
       (set! pc ee))]
    [`(conj-k ,g^ ,k^)                 (begin
                                         (set! k k^)
                                         (set! g g^) 
                                         (set! pc $append-map))]
;; This one is the interleave.
    [`($app-later-k ,$₂^ ,k^)          (begin
                                         (set! $₂ $)
                                         (set! $ $₂^)
                                         (set! k k^) 
                                         (set! pc $append))]
    [`($app-cons-k ,a^ ,k^)            (begin
                                         (set! $ (now (cons a^ $)))
                                         (set! k k^)
                                         (set! pc apply-k))]
    [`($app-map-th ,g^ ,k^)            (begin
                                         (set! $ (conj-k g^ $))
                                         (set! k k^)
                                         (set! pc apply-k))]
    [`($app-th ,$₂^ ,k^)               (begin
                                         (set! $ ($app-later-k $₂^ $))
                                         (set! k k^)
                                         (set! pc apply-k))]
    [`(mat-len-later-k ,k^)        (begin
;;                                         (set! n n^)
                                         (set! k k^)
                                         (set! pc mature-to-length))]
    ;; [`(pull-k ,k^)                     (begin
    ;;                                      (set! k k^)
    ;;                                      (set! pc pull))]
    [`(appendo-th)                     (begin
                                         (set! k $)
                                         (set! g `(disj (conj succeed succeed)
                                                        (conj succeed (appendo))))
                                         (set! pc ee))]
    [`(unproductiveo-th)               (begin
                                         (set! k $)
                                         (set! g `(unproductiveo))
                                         (set! pc ee))]
    [`(alwayso-th)                     (begin
                                         (set! k $)
                                         (set! g `(disj succeed (alwayso)))
                                         (set! pc ee))]))

(trace-define (ee) ;; g k
  (match g
    [`(disj ,g₁ ,g₂)  (begin
                        (set! g g₂)
                        (set! k (disj-outer-k g₁ k))
                        (set! pc ee))]
    [`(conj ,g₁ ,g₂)  (begin
                        (set! g g₁)
                        (set! k (conj-k g₂ k))
                        (set! pc ee))]
    [`succeed         (begin 
                        (set! $ (now (cons '#t (now '()))))
                        (set! pc apply-k))]
    [`fail            (begin
                        (set! $ (now '()))
                        (set! pc apply-k))]
    [`(appendo)       (begin
                        (set! $ (later (appendo-th)))
                        (set! pc apply-k))]
    [`(unproductiveo) (begin
                        (set! $ (later (unproductiveo-th)))
                        (set! pc apply-k))]
    [`(alwayso)       (begin
                        (set! $ (later (alwayso-th)))
                        (set! pc apply-k))]))

(trace-define ($append-map) ;; g $ k
  (match $
    [(now c)
     (match c
       ['()         (begin
;;                      (set! $ (now c))
                      (set! pc apply-k))]
       [(cons a $d) (begin
                      (set! k (disj-outer-k g k))
                      (set! $ $d) 
                      (set! pc $append-map))])]
    [(later c)      (begin
                      (set! $ (later ($app-map-th g c)))
                      (set! pc apply-k))]))

(trace-define ($append) ;; $ $₂ k
  (match $
    [(now c) 
     (match c
       ['()         (begin
                      (set! $ $₂)
                      (set! pc apply-k))]
       [(cons a $d) (begin
                      (set! k ($app-cons-k a k))
                      (set! $ $d) 
                      (set! pc $append))])]
    [(later c)      (begin
                      (set! $ (later ($app-th $₂ c)))
                      (set! pc apply-k))]))

;; (trace-define (pull) ;; $ k
;;   (match $
;;     [(now c)   (begin
;; ;;                 (set! $ (now c))
;;                  (set! pc apply-k))]
;;     [(later c) (begin
;;                  (set! $ (pull-k k))
;;                  (set! k c)
;;                  (set! pc apply-k))]))

(trace-define (mature-to-length);; n $ k
  (cond
    [(zero? n)         (set! pc apply-k)]
    [else 
     (match $
       [(now c)
        (match c
          ['()         (begin
;;                         (set! $ (now '()))
                         (set! pc apply-k))]
          [(cons a $d) (begin
                         (set! n (sub1 n))
                         (set! $ $d)
                         (set! k ($app-cons-k a k)) 
                         (set! pc mature-to-length))])]
       [(later c)      (begin
                         (set! $ (mat-len-later-k k))
                         (set! k c)
                         (set! pc apply-k))])]))

;; (trace-define (mature $)
;;   (match $
;;     [`(now ,$)
;;      (match $
;;        ['() (now '())]
;;        [(cons a $) (now (cons a (mature (force $))))])]
;;     [`(later ,$) (mature (force $))]))

(trace-define (trampoline)
  (pc)
  (trampoline))

(begin
  (set! n 3)
  (set! g '(appendo))
  (set! k (mat-len-later-k (empty-k)))
  (set! pc ee)
  (trampoline))
