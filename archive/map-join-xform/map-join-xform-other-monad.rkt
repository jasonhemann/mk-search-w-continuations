#lang racket

(define (ee exp s)
  (match exp
    [`(ore ,g1 ,g2)  (disj (ee g1 s) (ee g2 s))]
    [`(ande ,g1 ,g2) (bind (ee g1 s) (λ (s) (ee g2 s)))]
    [`(succeed)      (unit s)]
    [`(fail)         fail]
    [`(= ,t1 ,t2)    (match (unify t1 t2 s)
                       [`(Just s) (unit s)]
                       [`(None) fail])]
    [`(always ,t1)   (mdelay (ee `(disj (= ,t1 x) (alwayso ,t1)) s))]))

;; (define (mdelay e) 
;;   (λ () e))

(define (mdelay e)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (dk e)))))

;; could be g, could be (ee g)

(define (disj $1 $2)
  (match $1
    [(null? $1) $2]
    [(pair? $1) (cons (car $1) (disj (cdr $1) $2))]
    [(procedure? $1) (λ () (disj $2 ($1)))]))


;; (define (apply-dk dk c)
;;   (match c
;;     (else (dk c))))

;; (define (unit a)
;;   (λ (dk sk fk)
;;     (sk a fk)))

;; (define fail
;;   (λ (dk sk fk)
;;     (fk)))

;; (define (disj-dk dk c2)
;;   (λ (c)
;;     (apply-dk dk (disj c2 c))))

;; (define (disj c c2)
;;   (λ (dk sk fk)
;;     (c (disj-dk dk c2)
;;         sk
;;         (λ () (c2 dk sk fk)))))

;; (define (bind c f)
;;   (λ (dk sk fk)
;;     (c (λ (c) (dk (bind c f)))
;;        (λ (a fk) ((f a) dk sk fk))
;;        fk)))

;; ;; (define (map f m^)
;; ;;   (bind m^ (λ (a) (unit (f a)))))

;; ;; (define (join mm^)
;; ;;   (bind mm^ identity))

;; (define kons (λ (a fk) (cons a (fk))))
;; (define nill (λ () '()))
;; (define identity (λ (c) c))

(define (looper n)
  (if (zero? n)
      (λ (x) (x identity kons nill))
      (λ (x) (x (looper (sub1 n)) kons nill))))

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


;; ((looper 0) (disj (unit 5) fail))
;; ((looper 10) fail)
;; ((looper 30) (disj (a 20) (d 5)))
;; ((looper 30) (bind (a 5) b))
