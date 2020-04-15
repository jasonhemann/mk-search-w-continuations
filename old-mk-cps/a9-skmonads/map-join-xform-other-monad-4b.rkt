#lang racket

;; inlining functions used only once
;; notice that there's an issue with using delay the way we've done it. 

;; identity monad
;; (define id-unit (λ (x) x))
;; (define id-bind (λ (m f) (f m)))

;; Eval takes 4 arguments
;; computations take 4 arguments
;; sks take 3 arguments
;; dks take 2 arguments
;; fks take 1 arguments

(define (ee exp k)
  (match exp
    [`(ore ,g1 ,g2)  (ee g1 (λ (c) (ee g2 (λ (c2) (disj c c2 k)))))]
    [`(ande ,g1 ,g2) (ee g1 (λ (c) (bind c g2 k)))]
    [`(succeed)      (k (λ (dk sk fk k) (sk fk k)))]
    [`(fail)         (k (λ (dk sk fk k) (fk k)))]
    [`(always ,t1)
     (k (λ (dk sk fk k) (ee `(disj (= ,t1 x) (alwayso ,t1)) (λ (c) (dk c k)))))]))

(define (disj c c2 k)
  (k (λ (dk sk fk k)
       (c (λ (c k) (disj c2 c (λ (c) (dk c k))))
          sk
          (λ (k) (c2 dk sk fk k))
          k))))

(define (bind c g2 k)
  (k (λ (dk sk fk k)
       (c (λ (c k) (bind c g2 (λ (c) (dk c k))))
          (λ (fk k) (ee g2 (λ (c) (c dk sk fk k))))
          fk
          k))))

(define (looper n k)
  (if (zero? n)
      (k (λ (c k) (c <dk> <sk> <fk> k)))
      (k (λ (c k) (looper (sub1 n) (λ (dk) (c dk <sk> <fk> k)))))))

(looper <n> (λ (dk) (ee <exp> <s> (λ (c) (dk c <k>)))))

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

(define (looper n k)
  (if (zero? n)
      (k (λ (c k) (c <dk> <sk> <fk> k)))
      (k (λ (c k) (looper (sub1 n) (λ (dk) (c dk <sk> <fk> k)))))))

(looper <n> (λ (dk) (ee <exp> <s> (λ (c) (dk c <k>)))))

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
