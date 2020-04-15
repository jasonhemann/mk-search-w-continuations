#lang racket

(struct unit () #:transparent)

(define (ee g)
  (match g
    [`s (λ (sk)
          (λ (fk)
            ((sk (unit)) fk)))]
    [`f (λ (sk)
          (λ (fk)
            (fk)))]
    [`(d ,g1 ,g2)
     (λ (sk)
       (λ (fk)
         (ee g1)

         (ee g2)))]
    [`(c ,g1 ,g2) ]
    [`(allo)
     ((sk (λ (sk)
            (λ (fk)
              (((ee 
                 '(d s (allo)))
                sk)
               fk))))
      fk)]))

;; allo ideas
;; Compose the recursion w/fk
;; make the recursion an sk
;; add a third type of k

;; ee :: Expr -> (α -> (𝕀 -> β)) -> (𝕀 -> β) -> β
(define (ee g)
  (match g
    [`s (return (unit))]
    [`f (fail)]
    [`(d ,g1 ,g2)
     (λ (a) 
       (mplus ((ee g1) a) ((ee g2) a)))] ;; where does unit come in? 
    
    [`(c ,g1 ,g2) 
     (bind )
     
     
     ]
    [`(allo)
     ((sk (λ (sk)
            (λ (fk)
              (((ee 
                 '(d s (allo)))
                sk)
               fk))))
      fk)]))

(define (return a)
  (λ (sk)
    (λ (fk)
      (λ (dk)
        (((sk a) fk) dk)))))

(define (fail)
  (λ (sk)
    (λ (fk)
      (λ (dk)
        (fk)))))

(define (mplus c1 c2)
  (λ (sk)
    (λ (fk)
      (λ (dk)
        (((c1
           sk)
          (λ ()
            ((c2
              sk)
             fk)))
         (λ (d) ;; if the result is a delay,
           (dk ;; we return
            (λ (...) ;; a delay that
              ;; when executed,
              ;; begins the other computation
              ;; and continues the delayed computation
              ;; later              
              ))))))))

(define (bind c f)
  (λ (sk)
    (λ (fk)
      (λ (dk)
        
        ))))

;; New plan of attack:
;; Write the monadic semantics.
;; Write the list monad instantiation and check it
;; (Use Mitch's impl!)
;; Write the 2k impl.
;;

;; These could all be wrapped by a \a, and I think it'd still work
;; fine.

;; But I think we can consider this w/o it.

[[i]]          = return_m i
[[fail]]       = fail_m
[[disj e1 e2]] = append_m [[e1]] [[e2]]
[[conj e1 e2]] = bind [[e1]] (λ (_) [[e2]])
;; interesting how that looks like an fk
;; if we don't need the argument to the fn
[[appo]]       = later 'expr' ;; expression to evaluate. 

;; comparing base value to 
