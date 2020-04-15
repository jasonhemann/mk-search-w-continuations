#lang racket

;; inlining functions used only once 

(define (ee exp s k)
  (match exp
    [`(ore ,g1 ,g2)  (ee g1 s (λ (c) (ee g2 s (λ (c2) (disj c c2 k)))))]
    [`(ande ,g1 ,g2) (ee g1 s (λ (c) (bind c (λ (s k) (ee g2 s k)) k)))]
    [`(succeed)      (unit s)]
    [`(fail)         (fail k)]
    [`(always ,t1)   (mdelay (ee `(disj (= ,t1 x) (alwayso ,t1)) s) k)]))

;; (define (mdelay e) 
;;   (λ () e))

(define (mdelay e k)
  (k (λ (dk sk fk k)
       (dk e k))))

;; could be g, could be (ee g)

;; (define (disj $1 $2)
;;   (match $1
;;     [(null? $1) $2]
;;     [(pair? $1) (cons (car $1) (disj (cdr $1) $2))]
;;     [(procedure? $1) (λ () (disj $2 ($1)))]))

(define (unit s k)
  (k (λ (dk sk fk k)
       (sk s fk k))))

(define (fail k)
  (k (λ (dk sk fk k)
       (fk k))))

(define (disj c c2 k)
  (k (λ (dk sk fk k)
       (c (λ (c k) (disj c2 c (λ (c) (dk c))))
          sk
          (λ (k) (c2 dk sk fk k))
          k))))

(define (bind c f k)
  (k (λ (dk sk fk k)
       (c (λ (c k) (bind c f (λ (c) (dk c))))
          (λ (a fk k) (f a (λ (c) (c dk sk fk k))))
          fk
          k))))

;; ;; (define (map f m^)
;; ;;   (bind m^ (λ (a) (unit (f a)))))

;; ;; (define (join mm^)
;; ;;   (bind mm^ identity))

(define kons (λ (a fk k) (fk (λ (v) (k (cons a v))))))
(define nill (λ (k) (k '())))
(define identity (λ (c k) (k c)))

(define (looper n k)
  (match n
    [`Z (λ (c k) (c identity kons nill k))]
    [`(S ,n^)
     (λ (c k) (looper n^ (λ (dk) (c dk kons nill k))))]))

(ee '(ore (succeed) (fail)) '() (λ (c) (looper '(S (S Z)) (λ (dk) (dk c (λ (v) v))))))

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




;; ((looper 10) fail)
;; ((looper 30) (disj (a 20) (d 5)))
;; ((looper 30) (bind (a 5) b))
