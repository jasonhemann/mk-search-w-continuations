#lang racket
(require racket/trace)
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


(trace-define (ee exp s k)
  (match exp
    [`(ore ,g1 ,g2)  (ee g1 s (trace-lambda #:name fn (c) (ee g2 s
                                                              (trace-lambda #:name fn5 (c2) (disj c c2 k)))))]
    [`(ande ,g1 ,g2) (ee g1 s (trace-lambda #:name fn (c) (bind c g2 k)))]
    [`(succeed)      (k (trace-lambda #:name fn (dk sk fk k) (sk s fk k)))]
    [`(fail)         (k (trace-lambda #:name fn (dk sk fk k) (fk k)))]
    [`(alwayso)      (k (trace-lambda #:name fn (dk sk fk k) (ee `(fail) s
                                                                 (trace-lambda #:name fn0 (c) (display 'first) (dk c k)))))])) ;; must return a computation. 

(trace-define (disj c c2 k)
  (display 'in-this-fn)              
  (k 
   (trace-lambda #:name fn6 (dk sk fk k)
                 (c (trace-lambda #:name fn  (c k) (disj c2 c
                                                         (trace-lambda #:name fn  (c) (display 'second) (dk c k))))
        sk
        (trace-lambda #:name fn  (k) (c2 dk sk fk k))
        k))))

(trace-define (bind c g2 k)
  (k
   (trace-lambda #:name fn2 (dk sk fk k)
     (c (trace-lambda #:name fn  (c k) (bind c g2 (trace-lambda #:name fn  (c) (display 'third) (dk c k))))
        (trace-lambda #:name fn  (a fk k) (ee g2 a (trace-lambda #:name fn  (c) (c dk sk fk k))))
        fk
        k))))

(trace-define (looper n exp s dk sk fk k)
  (match n
    [`Z       (ee exp s (trace-lambda #:name fn  (c) (c dk sk fk k)))]
    [`(S ,n^) (ee exp s (trace-lambda #:name fn3 (c) (looper n^ exp s dk sk fk (trace-lambda #:name fn4 (dk) (c dk sk fk k)))))]))

(looper '(S Z) '(alwayso) '()
        (trace-lambda #:name fn  (c k) (k c))
        (trace-lambda #:name fn  (a d k)
                      (d (trace-lambda #:name fn (r) (k (cons a r)))))
        (trace-lambda #:name fn  (k) (k '()))
        (trace-lambda #:name fn1 (dk) dk))

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

;; (define (looper n k)
;;   (if (zero? n)
;;       (k (λ (c k) (c <dk> <sk> <fk> k)))
;;       (k (λ (c k) (looper (sub1 n) (λ (dk) (c dk <sk> <fk> k)))))))

;; (looper <n> (λ (dk) (ee <exp> <s> (λ (c) (dk c <k>)))))

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


;; (ee '(ore (succeed) (fail)) '() (λ (k) (looper 0 (λ (l) (l (λ (v) v))) )))

;; ((looper 10) fail)
;; ((looper 30) (disj (a 20) (d 5)))
;; ((looper 30) (bind (a 5) b))
