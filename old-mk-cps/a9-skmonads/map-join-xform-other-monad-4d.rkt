#lang racket

(define (ore-inner-dk c dk)
  (λ (c2) 
    (disj c c2 dk)))

(define (ore-outer-dk exp^^ s c dk)
  (λ (c) 
    (ee exp^^ s (ore-inner-dk c dk))))

(define (ande-dk c exp^^ dk)
  (λ (c)
    (bind c exp^^ dk)))

(define (dunno-dk dk k)
  (λ (c)
    (dk c k)))

(define (disj-dk c2 dk)
  (λ (c k)
    (disj c2 c (dunno-dk dk k))))

(define (disj-fk c2 dk sk fk)
  (λ (k) 
    (c2 dk sk fk k)))

(define (succeed-c s)
  (λ (dk sk fk k)
    (sk s fk k)))

(define (fail-c)
  (λ (dk sk fk k)
    (fk k)))

(define (alwayso-c s)
  (λ (dk sk fk k)
    (ee `(disj (succeed) (alwayso)) s (dunno-dk dk k))))

(define (disj-c c c2)
  (λ (dk sk fk k)
    (c (disj-dk c2 dk)
       sk
       (disj-fk c2 dk sk fk)
       k)))

(define (ee exp s dk)
  (match exp
    [`(ore ,exp^ ,exp^^)  (ee exp^ s (ore-outer-dk exp^^ s c dk))]
    [`(ande ,exp^ ,exp^^) (ee exp^ s (ande-dk c exp^^))]
    [`(succeed)           (dk (succeed-c s))]
    [`(fail)              (dk (fail-c))]
    [`(alwayso)           (dk (alwayso-c s))]))

(define (disj c c2 dk)
  (dk (disj-c c c2)))

(define (bind-dk exp dk)
  (λ (c k)
    (bind c exp (dunno-dk dk k))))

(define (bind-sk exp dk sk)
  (λ (s fk k)
    (ee exp s (resume-comp-dk dk sk fk k))))

;; Neat that this takes all the state in which to run a computation first,
;; and then later takes the computation to run
(define (resume-comp-dk dk sk fk k)
  (λ (c) 
    (c dk sk fk k)))

(define (bind c exp dk)
  (dk (λ (dk sk fk k)
        (c (bind-dk exp dk)
           (bind-sk exp dk sk)
           fk
           k))))

(define (looper n exp s dk sk fk k)
  (match n
    [`Z       (ee exp s (resume-comp-dk dk sk fk k))] ;; could be changed to a () base case.
    [`(S ,n^) (looper n^ exp s dk sk fk (λ (dk) (ee exp s (resume-comp-dk dk sk fk k))))]))

(looper '<n> '<exp> '<s> '<dk> '<sk> '<fk> '<k>)


;; (= ,t1 x)
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
