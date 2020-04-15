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
;;    [`($app-map-cons-inner-k ,$₂^ ,k^) ($append $ $₂^ k^)]
;;    [`($app-map-cons-outer-k ,g^ ,k^)  (ee g^ (disj-inner-k $ k^))]
;;    [`($app-map-later ,g^ ,k^)         ($append-map g^ $ k^)]
;;    [`(mat-len-cons-k ,a^ ,k^)         (apply-k k^ (now (cons a^ $)))]
    [`(empty-k) $]
    [`(disj-inner-k ,$₂^ ,k^)          (let* ((k k^)
                                              ($₂ $₂^))
                                         ($append $ $₂ k))]
    [`(disj-outer-k ,g^ ,k^)           (let* ((k (disj-inner-k $ k^))
                                              (g g^)) 
                                         (ee g k))]
    [`(conj-k ,g^ ,k^)                 (let* ((g g^)
                                              (k k^)) 
                                         ($append-map g $ k))]
;; This one is the interleave.
    [`($app-later-k ,$₂^ ,k^)          (let* (($₂ $)
                                              ($ $₂^)
                                              (k k^)) 
                                         ($append $ $₂ k))]
    [`($app-cons-k ,a^ ,k^)            (let* (($ (now (cons a^ $)))
                                              (k k^))
                                         (apply-k k $))]
    [`($app-map-th ,g^ ,k^)            (let* (($ (conj-k g^ $))
                                              (k k^))
                                         (apply-k k $))]
    [`($app-th ,$₂^ ,k^)               (let* (($ ($app-later-k $₂^ $))
                                              (k k^))
                                         (apply-k k $))]
    [`(mat-len-later-k ,n^ ,k^)        (let* ((n n^)
                                              (k k^)) 
                                         (mature-to-length n $ k))]
    [`(pull-k ,k^)                     (let* ((k k^)) 
                                         (pull $ k))]
    [`(outer-init-k)                   (let* ((k (inner-init-k))) 
                                         (pull $ k))]
    [`(inner-init-k)                   (let* ((n 3)
                                              (k (empty-k)))
                                         (mature-to-length n $ k))]
    [`(appendo-th)                     (let* ((k $)
                                              (g `(disj (conj succeed succeed)
                                                        (conj succeed (appendo)))))
                                         (ee g k))]
    [`(unproductiveo-th)               (let* ((k $)
                                              (g `(unproductiveo)))
                                         (ee g k))]
    [`(alwayso-th)                     (let* ((k $)
                                              (g `(disj succeed (alwayso))))
                                         (ee g k))]))

(define (ee g k)
  (match g
    [`(disj ,g₁ ,g₂)  (let* ((g g₂)
                             (k (disj-outer-k g₁ k)))
                        (ee g k))]
    [`(conj ,g₁ ,g₂)  (let* ((g g₁)
                             (k (conj-k g₂ k)))
                        (ee g k))]
    [`succeed         (let* (($ (now (cons '#t (now '())))))
                        (apply-k k $))]
    [`fail            (let* (($ (now '())))
                        (apply-k k $))]
    [`(appendo)       (let* (($ (later (appendo-th))))
                        (apply-k k $))]
    [`(unproductiveo) (let* (($ (later (unproductiveo-th))))
                        (apply-k k $))]
    [`(alwayso)       (let* (($ (later (alwayso-th))))
                        (apply-k k $))]))

(define ($append-map g $ k)
  (match $
    [(now c)
     (match c
       ['()         (let* (($ (now c)))
                      (apply-k k $))]
       [(cons a $d) (let* (($ $d)
                           (k (disj-outer-k g k))) 
                      ($append-map g $ k))])]
    [(later c)      (let* (($ (later ($app-map-th g c))))
                      (apply-k k $))]))

(define ($append $ $₂ k)
  (match $
    [(now c) 
     (match c
       ['()         (let* (($ $₂))
                      (apply-k k $))]
       [(cons a $d) (let* (($ $d)
                           (k ($app-cons-k a k))) 
                      ($append $ $₂ k))])]
    [(later c)      (let* (($ (later ($app-th $₂ c))))
                      (apply-k k $))]))

(define (pull $ k)
  (match $
    [(now c)   (let* (($ (now c)))
                 (apply-k k $))]
    [(later c) (let* (($ (pull-k k))
                      (k c))
                 (apply-k k $))]))

(define (mature-to-length n $ k)
  (cond
    [(zero? n)         (apply-k k $)]
    [else 
     (match $
       [(now c)
        (match c
          ['()         (let* (($ (now '())))
                         (apply-k k $))]
          [(cons a $d) (let* ((n (sub1 n))
                              ($ $d)
                              (k ($app-cons-k a k))) 
                         (mature-to-length n $ k))])]
       [(later c)      (let* (($ (mat-len-later-k n k))
                              (k c))
                         (apply-k k $))])]))

;; (define (mature $)
;;   (match $
;;     [`(now ,$)
;;      (match $
;;        ['() (now '())]
;;        [(cons a $) (now (cons a (mature (force $))))])]
;;     [`(later ,$) (mature (force $))]))

(let* ((g '(appendo))
       (k (outer-init-k)))
  (ee g k))


