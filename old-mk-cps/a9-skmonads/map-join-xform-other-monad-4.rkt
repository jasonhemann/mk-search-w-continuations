#lang racket

;; We can merge the fks with the ks
;; I mean, into the same dispatch table. 

(define (disj-c c c2) `(disj-c ,c ,c2))
(define (bind-c c g) `(bind-c ,c ,g))
(define (unit) `(unit))
(define (fail) `(fail))
(define (always-c t1) `(always-c ,t1))

(define (disj-dk c2 dk) `(disj-dk ,c2 ,dk))
(define (bind-dk g dk) `(bind-dk ,g ,dk))
(define (looper-iter-dk n) `(looper-iter-dk ,n))
(define (loop-base-dk) `(loop-base-dk))
(define (bind-sk g dk sk) `(bind-sk ,g ,dk ,sk))

(define (disj-fk c2 dk sk fk) `(disj-fk ,c2 ,dk ,sk ,fk))
(define (or-inner-k c2 k) `(or-inner-k ,c2 ,k))
(define (or-outer-k g2 k) `(or-outer-k ,g2 ,k))
(define (and-k g2 k) `(and-k ,g2 ,k))
(define (delay-k dk k) `(delay-k ,dk ,k))
(define (bind-k dk sk fk k) `(bind-k ,dk ,sk ,fk ,k))
(define (recur-dk-k c^ k) `(recur-dk-k ,c^ ,k))
(define (init-k exp k) `(init-k ,exp ,k))

(define (apply-c c dk sk fk k)
  (match c
    [`(fail)           (apply-k fk k)]
    [`(unit)           (apply-sk sk fk k)]
    [`(disj-c ,c^ ,c2) (apply-c c^ (disj-dk c2 dk) sk (disj-fk c2 dk sk fk) k)]
    [`(bind-c ,c^ ,g)  (apply-c c^ (bind-dk g dk) (bind-sk g sk dk) fk k)]
    [`(always-c ,t1)   (ee `(disj (= ,t1 x) (alwayso ,t1)) (delay-k dk k))]
    ;; (else (c dk sk fk k))
    ))

(define (apply-sk dk c k)
  (match dk
    [`(loop-base-dk dk^ sk fk)         (apply-c c dk^ sk fk k)]
    [`(disj-dk ,c2 ,dk^)               (apply-k (delay-k dk^ k) (disj-c c c2))]
    [`(looper-iter-dk ,n ,exp ,sk ,fk) (looper n exp (recur-dk-k c sk fk k) sk fk) ]
    [`(bind-dk ,g ,dk^)                (apply-k (delay-k dk^ k) (bind-c c g))]
    [`(bind-sk ,g ,dk^ ,sk)            (ee g (bind-k dk^ sk c k))]
    ;; (else (dk c k))
    ))

(define (apply-k k c)
  (match k
    [`(bind-k     ,dk ,sk ,fk ,k^) (apply-c c  dk sk fk k^)]
    [`(disj-fk    ,c2 ,dk ,sk ,fk) (apply-c c2 dk sk fk c)]
    [`(recur-dk-k ,c^ ,sk ,fk ,k^) (apply-c c^ c  sk fk k^)]
    [`(or-inner-k ,c2 ,k^)         (apply-k k^ (disj-c c2 c))]
    [`(and-k ,g2 ,k^)              (apply-k k^ (bind-c c g2))]
    [`(delay-k ,dk ,k^)            (apply-k dk c k^)]
    [`(or-outer-k ,g2 ,k^)         (ee g2 (or-inner-k c k^))]
    [`(init-k ,exp^ ,k^)           (ee exp^ (delay-k c k^))]
    ;; (else (k c))
    ))

(define (ee exp k)
  (match exp
    [`(ore ,g1 ,g2)  (ee g1 (or-outer-k g2 k))]
    [`(ande ,g1 ,g2) (ee g1 (and-k g2 k))]
    [`(succeed)      (apply-k k (unit))]
    [`(fail)         (apply-k k (fail))]
    [`(always ,t1)   (apply-k k (always-c t1))]))

(define (looper n exp dk sk fk k)
  (match n
    [`Z        (ee exp (delay-k (loop-base-dk dk sk fk) k))]
    [`(S ,n^)  (ee exp (delay-k (looper-iter-dk n^ exp sk fk) k))]))

(looper <n> <exp> <dk> <sk> <fk> <k>)

;; (ee <exp> (λ (c) ))
;; ((looper 30) (ee '(disj (a 20) (d 5)) '()))
;; (looper 30 (λ (dk) (ee '(disj (a 20) (d 5)) '() (λ (v) (dk v (empty-k))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (a n)
;;   (λ (dk sk fk)
;;     (apply-dk dk (b n))))

;; (define (b n)
;;   (λ (dk sk fk)
;;     (apply-dk dk (c n))))

;; (define (c n)
;;   (λ (dk sk fk)
;;     (apply-dk dk (disj (unit n) (a (add1 n))))))

;; (define (d n)
;;   (λ (dk sk fk)
;;     (apply-dk dk (e n))))

;; (define (e n)
;;   (λ (dk sk fk)
;;     (apply-dk dk (disj (unit n) (d (add1 n))))))


(ee '(ore (succeed) (fail)) '() (λ (k) (looper 0 (λ (l) (l (λ (v) v))) ))) 

;; ((looper 10) fail)
;; ((looper 30) (disj (a 20) (d 5)))
;; ((looper 30) (bind (a 5) b))

;; (define (looper n k)
;;   (if (zero? n)
;;       (k (λ (c k) (c <dk> <sk> <fk> k)))
;;       (k (λ (c k) (looper (sub1 n) (λ (dk) (c dk <sk> <fk> k)))))))

;; (looper <n> (λ (dk) (ee <exp> <s> (λ (c) (dk c <k>)))))


;; (define (mdelay e) 
;;   (λ () e))

;; (define (mdelay e k)
;;   (k (λ (dk sk fk k)
;;        (dk e k))))

;; could be g, could be (ee g)

;; (define (disj $1 $2)
;;   (match $1
;;     [(null? $1) $2]
;;     [(pair? $1) (cons (car $1) (disj (cdr $1) $2))]
;;     [(procedure? $1) (λ () (disj $2 ($1)))]))

;; (define (unit s k)
;;   (k (λ (dk sk fk k)
;;        (sk s fk k))))

;; (define (fail k)
;;   (k (λ (dk sk fk k)
;;        (fk k))))

;; ;; (define (map f m^)
;; ;;   (bind m^ (λ (a) (unit (f a)))))

;; ;; (define (join mm^)
;; ;;   (bind mm^ identity))

;; (define kons (λ (a fk) (cons a (fk))))
;; (define nill (λ () '()))
;; (define identity (λ (c) c))
;; (define (bind c f k)
;;   (k (bind-c c f)))
