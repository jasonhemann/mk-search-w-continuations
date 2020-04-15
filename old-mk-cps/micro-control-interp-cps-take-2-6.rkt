#lang racket
(define k 'hukarz)
(define v 'hukarz)
(define $ 'hukarz)
(define $2 'hukarz)
(define g 'hukarz)
(define n 'hukarz)

(define (one? n) (eqv? n 1))
(define (monus1 n) (if (zero? n) n (sub1 n)))
(define (ae r) (cdr (assv r init-env)))
(define (delay? th) (eqv? (car th) 'th))

;; (define (ep-inner-k n^ k^)                   `(ep-inner-k ,n^ ,k^))
;; (define (ep-outer-k k^)                      `(ep-outer-k ,k^))
;; (define (disj-inner-k $^ k^)                 `(disj-inner-k ,$^ ,k^))
;; (define ($append-map-cons-inner-k $v^ k^)    `($append-map-cons-inner-k ,$v^ ,k^))
;; (define (take-inner-k a^ k^)                 `(take-inner-k ,a^ ,k^))
;; (define (conj-k g^ k^)                       `(conj-k ,g^ ,k^))
(define (empty-k)                            `(empty-k))
(define (take-k n^ k^)                       `(take-k ,n^ ,k^))
(define (pull-k k^)                          `(pull-k ,k^))
(define ($append-proc-force-k $^ k^)         `($append-proc-force-k ,$^ ,k^))
(define ($append-cons-k a^ k^)               `($append-cons-k ,a^ ,k^))
(define ($append-map-proc-k g^ k^)           `($append-map-proc-k ,g^ ,k^))
(define ($append-map-cons-outer-k $^ g^ k^)  `($append-map-cons-outer-k ,$^ ,g^ ,k^))
(define (disj-outer-k g^ k^)                 `(disj-outer-k ,g^ ,k^))
(define ($append-th $1^ $2^)    `(th $append-th ,$1^ ,$2^))
(define ($append-map-th $^ g^)  `(th $append-map-th ,$^ ,g^))
(define (listo-th)              `(th listo-th))
(define (unproductiveo-th)      `(th unproductiveo-th))



(define (apply-k k v)
  (match k
    [`(empty-k)                              v]
    [`(take-k ,n^ ,k^)                       (let* ((n n^)
                                                    ($ v)
                                                    (k k^))
                                               (take n $ k))]
    [`(pull-k ,k^)                           (let* (($ v)
                                                    (k k^))
                                               (pull $ k))]
    [`($append-proc-force-k ,$^ ,k^)         (let* (($2 v)
                                                    ($ $^)
                                                    (k k^)) 
                                               ($append $ $2 k))]
    [`($append-cons-k ,a^ ,k^)               (let* ((k k^)
                                                    (v (cons a^ v)))
                                               (apply-k k v))]
    [`($append-map-proc-k ,g^ ,k^)           (let* (($ v)
                                                    (g g^)
                                                    (k k^))
                                               ($append-map $ g k))]
    [`($append-map-cons-outer-k ,$^ ,g^ ,k^) (let* ((k ($append-proc-force-k v k^))
                                                    (g g^)
                                                    ($ $^)) 
                                               ($append-map $ g k))]
    [`(disj-outer-k ,g^ ,k^)                 (let* ((k ($append-proc-force-k v k^))
                                                    (g g^))
                                               (ee g k))]
;;  [`(ep-inner-k ,n^ ,k^)                   (take n^ v k^)]
;;  [`(ep-outer-k ,k^)                       (pull v k^)]
;;  [`(disj-inner-k ,$^ ,k^)                 ($append $^ v k^)]
;;  [`($append-map-cons-inner-k ,$^ ,k^)     ($append $^ v k^)]
;;  [`(take-inner-k ,a^ ,k^)                 (apply-k k^ (cons a^ v))]
;;  [`(conj-k ,g^ ,k^)                       ($append-map v g^ k^)]
;;  [else (k v)]
  ))

(define (force $ k)
  (match $
    [`(th $append-th ,$1^ ,$2^)   (let* ((k ($append-proc-force-k $2^ k))
                                         ($ $1^)) 
                                    (force $ k))]
    [`(th $append-map-th ,$^ ,g^) (let* ((k ($append-map-proc-k g^ k))
                                         ($ $^)) 
                                    (force $ k))]
    [`(th listo-th)               (let* ((g '(disj (conj succeed succeed) (listo)))) 
                                    (ee g k))]
    [`(th unproductiveo-th)       (let* ((g '(unproductiveo)))
                                    (ee g k))]
    ;; (else ($ k*))
    ))

(define (ee g k)
  (match g
    [`succeed        (let* ((v '(#t)))
                       (apply-k k v))]
    [`fail           (let* ((v '()))
                       (apply-k k v))]
    [`(disj ,g1 ,g2) (let* ((k (disj-outer-k g2 k))
                            (g g1)) 
                       (ee g k))]
    [`(conj ,g1 ,g2) (let* ((k ($append-map-proc-k g2 k))
                            (g g1)) 
                       (ee g k))]
    [`(,r)           (let* ((v (ae r)))
                       (apply-k k v))]))

(define ($append $ $2 k)
  (cond
    ((null? $)      (let* ((v $2)) 
                      (apply-k k v)))
    ((delay? $)     (let* ((v ($append-th $ $2))) 
                      (apply-k k v)))
    ((cons? $)      (let* ((k ($append-cons-k (car $) k))
                           ($ (cdr $))) 
                      ($append $ $2 k)))))

(define ($append-map $ g k)
  (cond
    ((null? $)      (let* ((v $)) 
                      (apply-k k v)))
    ((delay? $)     (let* ((v ($append-map-th $ g))) 
                      (apply-k k v)))
    ((cons? $)      (let* ((k ($append-map-cons-outer-k (cdr $) g k))) 
                      (ee g k)))))

(define (pull $ k)
  (cond
    ((null? $)  (let* ((v $)) 
                  (apply-k k v)))
    ((delay? $) (let* ((k (pull-k k))) 
                  (force $ k)))
    ((cons? $)  (let* ((v $)) 
                  (apply-k k v)))))

(define (take n $ k)
  (cond
    ((null? $) (let* ((v $)) 
                 (apply-k k v)))
    ((one? n)  (let* ((v (cons (car $) '()))) 
                 (apply-k k v)))
    (else      (let* ((k (take-k (monus1 n) ($append-cons-k (car $) k)))
                      ($ (cdr $))) 
                 (pull $ k)))))

(define init-env `((listo . ,(listo-th)) (unproductiveo . ,(unproductiveo-th))))

(let* ((k (pull-k (take-k 5 (empty-k))))
       (g '(listo))) 
  (ee g k))


