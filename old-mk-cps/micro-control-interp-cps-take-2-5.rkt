#lang racket

(define (one? n) (eqv? n 1))
(define (monus1 n) (if (zero? n) n (sub1 n)))
(define (ae r) (cdr (assv r init-env)))
(define (delay? th) (eqv? (car th) 'th))

;; (define rns '(listo))
;; (define rbs '((disj succeed (listo))))
;; (define init-env
;;   (map (λ (name exp)
;;          `(,name . ,(λ (k) (ee exp k))))
;;        rns
;;        rbs))


;; (define (ep-inner-k n^ k^)                   `(ep-inner-k ,n^ ,k^))
;; (define (ep-outer-k k^)                      `(ep-outer-k ,k^))
;; (define (disj-inner-k $^ k^)                 `(disj-inner-k ,$^ ,k^))
;; (define ($append-map-cons-inner-k $v^ k^)    `($append-map-cons-inner-k ,$v^ ,k^))
;; (define (take-inner-k a^ k^)                 `(take-inner-k ,a^ ,k^))
;; (define (conj-k g^ k^)                       `(conj-k ,g^ ,k^))
(define (empty-k)                            `(empty-k))
(define (take-outer-k n^ k^)                 `(take-outer-k ,n^ ,k^))
(define (pull-k k^)                          `(pull-k ,k^))
(define ($append-proc-force-k $^ k^)         `($append-proc-force-k ,$^ ,k^))
(define ($append-cons-k a^ k^)               `($append-cons-k ,a^ ,k^))
(define ($append-map-proc-k g^ k^)           `($append-map-proc-k ,g^ ,k^))
(define ($append-map-cons-outer-k $^ g^ k^)  `($append-map-cons-outer-k ,$^ ,g^ ,k^))
(define (disj-outer-k g^ k^)                 `(disj-outer-k ,g^ ,k^))

(define (apply-k k v)
  (match k
  [`(empty-k)                              v]
  [`(take-outer-k ,n^ ,k^)                 (take n^ v k^)]
  [`(pull-k ,k^)                           (pull v k^)]
  [`($append-proc-force-k ,$^ ,k^)         ($append $^ v k^)]
  [`($append-cons-k ,a^ ,k^)               (apply-k k^ (cons a^ v))]
  [`($append-map-proc-k ,g^ ,k^)           ($append-map v g^ k^)]
  [`($append-map-cons-outer-k ,$^ ,g^ ,k^) ($append-map $^ g^ ($append-proc-force-k v k^))]
  [`(disj-outer-k ,g^ ,k^)                 (ee g^ ($append-proc-force-k v k^))]
;;  [`(ep-inner-k ,n^ ,k^)                   (take n^ v k^)]
;;  [`(ep-outer-k ,k^)                       (pull v k^)]
;;  [`(disj-inner-k ,$^ ,k^)                 ($append $^ v k^)]
;;  [`($append-map-cons-inner-k ,$^ ,k^)     ($append $^ v k^)]
;;  [`(take-inner-k ,a^ ,k^)                 (apply-k k^ (cons a^ v))]
;;  [`(conj-k ,g^ ,k^)                       ($append-map v g^ k^)]
;;  [else (k v)]
  ))

(define ($append-th $1^ $2^)    `(th $append-th ,$1^ ,$2^))
(define ($append-map-th $^ g^)  `(th $append-map-th ,$^ ,g^))
(define (listo-th)              `(th listo-th))
(define (unproductiveo-th)      `(th unproductiveo-th))

(define (force $ k*)
  (match $
    [`(th $append-th ,$1^ ,$2^)   (force $1^ ($append-proc-force-k $2^ k*))]
    [`(th $append-map-th ,$^ ,g^) (force $^ ($append-map-proc-k g^ k*))]
    [`(th listo-th)               (ee '(disj (conj succeed succeed) (listo)) k*)]
    [`(th unproductiveo-th)       (ee '(unproductiveo) k*)]
    ;; (else ($ k*))
    ))

(define (ee e k)
  (match e
    [`succeed        (apply-k k '(#t))]
    [`fail           (apply-k k '())]
    [`(disj ,g1 ,g2) (ee g1 (disj-outer-k g2 k))]
    [`(conj ,g1 ,g2) (ee g1 ($append-map-proc-k g2 k))]
    [`(,r)           (apply-k k (ae r))]))

(define ($append $ $2 k)
  (cond
    ((null? $)      (apply-k k $2))
    ((delay? $)     (apply-k k ($append-th $ $2)))
    ((cons? $)      ($append (cdr $) $2 ($append-cons-k (car $) k)))))

(define ($append-map $ g k)
  (cond
    ((null? $)      (apply-k k '()))
    ((delay? $)     (apply-k k ($append-map-th $ g)))
    ((cons? $)      (ee g ($append-map-cons-outer-k (cdr $) g k)))))

(define (pull $ k)
  (cond
    ((null? $)  (apply-k k $))
    ((delay? $) (force $ (pull-k k)))
    ((cons? $)  (apply-k k $))))

(define (take n $ k)
  (cond
    ((null? $) (apply-k k '()))
    ((one? n)  (apply-k k (cons (car $) '())))
    (else      (pull (cdr $) (take-outer-k (monus1 n) ($append-cons-k (car $) k))))))

(define init-env `((listo . ,(listo-th)) (unproductiveo . ,(unproductiveo-th))))

(ee '(listo) (pull-k (take-outer-k 5 (empty-k))))


