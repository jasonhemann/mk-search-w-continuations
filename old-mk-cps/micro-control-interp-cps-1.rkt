#lang racket

;; (define rns '(listo))
;; (define rbs '((disj succeed (listo))))
;; (define init-env
;;   (map (λ (name exp)
;;          `(,name . ,(λ (k) (ee exp k))))
;;        rns
;;        rbs))

(define init-env `((listo . ,(λ (k) (ee '(disj (conj succeed succeed) (listo)) k)))
                   (unproductiveo . ,(λ (k) (ee '(unproductiveo) k)))))

(define (apply-k k v)
  (match k
    (else (k v))))

(define (ep q n k)
  (ee q
    (λ ($)
      (pull $ 
        (λ (m$)
          (take n m$ k))))))

(define (ee e k)
  (match e
    [`succeed (apply-k k '(#t))]
    [`fail (apply-k k '())]
    [`(disj ,g1 ,g2)
     (ee g1 
       (λ ($1)
         (ee g2 
           (λ ($2) 
             ($append $1 $2 k)))))]
    [`(conj ,g1 ,g2)
     (ee g1 
       (λ ($)
         ($append-map $ g2 k)))]
    [`(,r) (apply-k k (ae r))]))

(define (ae r) (cdr (assv r init-env)))

(define ($append $1 $2 k)
  (cond
    ((null? $1) (apply-k k $2))
    ((cons? $1)
     ($append (cdr $1) $2
       (λ ($r)
         (apply-k k (cons (car $1) $r)))))
    ((procedure? $1)
     (apply-k k 
       (λ (k^)
         (force $1
           (λ ($1^)
             ($append $2 $1^ k^))))))))

(define (force $ k)
  (match $
    (else ($ k))))

(define ($append-map $ g k)
  (cond
    ((null? $) (apply-k k '()))
    ((cons? $)
     ($append-map (cdr $) g 
        (λ ($w)
          (ee g 
            (λ ($v)
              ($append $v $w k))))))
    ((procedure? $) 
     (apply-k k 
       (λ (k) 
         (force $ 
           (λ ($^)
             ($append-map $^ g k))))))))

(define (pull $ k) 
  (if (procedure? $)
      (force $ (λ (v) (pull v k)))
      (apply-k k $)))

(define (take n $ k)
  (cond
    ((null? $) (apply-k k '()))
    ((and n (zero? (sub1 n))) 
     (apply-k k (cons (car $) '())))
    (else
     (pull (cdr $) 
       (λ (m$)
         (take (and n (sub1 n)) m$ 
           (λ (res)
             (apply-k k (cons (car $) res)))))))))

(define (empty-k)
  (λ (v) v))

(ep '(unproductiveo) 0 (empty-k))
