#lang racket

;; Here I'll substitute in the sk/fk model to begin with.
;; Uncurrying to try and make type mistakes syntactic. 

;; This should get me close to where I need to be, and then I should
;; be able to have in a non-tail position still some work to be done
;; with the results.

;; Might be that it would help to try and undo the monadic work here,
;; directly in-line the parts that I understand, and then back into
;; the double-continuation operations.

;; Not clear if currying will ultimately prove useful or not. 

;; (define (eval g s)
;;   (match g
;;     [`(succeed) (return s)]
;;     [`(fail) (mzero)]
;;     [`(^ ,g1 ,g2) (bind (eval g1 s) (λ (s) (eval g2 s)))] 
;;     [`(v ,g1 ,g2) (mplus (eval g1 s) (eval g2 s))]
;;     [`(threeo) (λ () (eval '(twoo) s))]
;;     [`(twoo)   (λ () (eval '(one) s))]
;;     [`(oneo)   (λ () (eval '(succeed) s))]
;;     [`(alwayso) (λ () (eval '(v (succeed) (alwayso)) s))]))

;; Basic interpreter, doesn't do anything, returns no useful value, unit
;; Identity monad
;; (define (eval g)
;;   (match g
;;     [`(succeed) (return '_)]))

;; So, I don't need to go all the way through the exercise
;; Trying this out with the reader, or state, is enough to tell me
;; what I need to know about the conj operation. That there are
;; multiple binds required

;; (define (eval g)
;;   (match g
;;     [`(succeed) (return '_)]
;;     [`(addto ,t) (put t)]
;;     [`(^ ,g1 ,g2)
;;      (do
;;          (eval g2)
;;          (s <- (with-state)) ;; not sure how I feel here
;;          (eval g2 s))]))

;; Unclear what order they should come in. Should it be Delay over State ?
;; State over Delay? 


;; With LP state, I don't know that I know the difference between the pure and the state value.
;; It seems like what I want out *is* the impure value.
;; So what's the pure?
;; Certainly it can be just a special case of where the pure value doesn't matter.

;; Remove the state! #2020

;; Remove the need for (\s ...) throughout, and simplify down further. 



(define (eval g)
  (match g
    [`,n #:when (number? n) (λ (dk sk fk) ((sk n) fk))] ;; (lift return-state) a ? 
    [`(fail)                (λ (dk sk fk) (fk))]
    [`(^ ,g1 ,g2)
     ;; (bind (eval g1)
     ;;             (λ (_) 
     ;;               (eval g2)))
     ;; Above, the version using only sequencing
                            (λ (dk sk fk)
                              ((eval g1)
                               (λ (m^) (dk (bind m^ (eval g2)) ? ?))
                               (λ (b) (λ (fk)
                                        ;; building blocks
                                        (eval g2)
                                        b
                                        dk
                                        sk
                                        fk))
                               fk))]

    [`(v ,g1 ,g2)
     ;; The sk/fk version.
     ;; Why is the fk lambda (), if it's the result of a CPS xform?
     ;; Optimization, b/c we don't use the previous value in the fk? 
     ;; (lambda (sk) (lambda (fk) (g1 sk (lambda () (g2 sk fk)))))
                            (λ (dk sk fk)
                              ((eval g1)
                               (λ (c1^) (dk (mplus (eval g2) c1^)))
                               sk
                               (λ () ((eval g2) dk sk fk))))]
    [`(threeo)              (λ (dk sk fk) (dk (eval '(twoo))))] ; the (dk ...) is suspicious
    [`(twoo)                (λ (dk sk fk) (dk (eval '(one))))]
    [`(oneo)                (λ (dk sk fk) (dk (eval '(succeed))))]
    [`(alwayso)             (λ (dk sk fk) (dk (eval '(v (succeed) (alwayso)))))]))

;; (define-syntax-rule (define-relation (n . args) g)
;;   (define (n . args)
;;     (λ (dk sk fk)
;;       (dk g))))

;; (define (return a)
;;   (λ (dk sk fk) ((sk a) fk)))

;; (define (bind m f)
;;   (λ (dk sk fk)
;;     (m (λ (m^) (dk (bind m^ f) ? ?))
;;        (λ (b) (λ (fk) ((f b) dk sk fk)))
;;        fk)))

;; (define (mzero)
;;   (λ (dk sk fk) (fk)))

;; (define (mplus m1 m2)
;;   (λ (dk sk fk)
;;     (m1
;;      (λ (c1^)
;;        (dk (mplus m2 c1^)))
;;      sk
;;      (λ ()
;;        (m2 dk sk fk)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define ($append $1 $2)
;;   (cond
;;     [(null? $1) $2]
;;     [(procedure? $1) (λ () ($append $2 ($1)))]
;;     [(pair? $1) (cons (car $1) ($append (cdr $1) $2))]))

;; (define ($append-map $1 f) ;; rename to g, if it's a goal
;;   (cond
;;     [(null? $1) '()]
;;     [(procedure? $1) (λ () ($append-map $1 f))] 
;;     [(pair? $1) ($append (f (car $1)) ($append-map (cdr $1) f))])) ;; (f (car $1)) or (eval (car $1) f)

;; (define (unit s)
;;   (list s))

;; (define (mzero)
;;   (list))

;; (define (bind $ f)
;;   ($append-map $ f))

;; (define (mplus $1 $2)
;;   ($append $1 $2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We also do not have a `run` for the monad's implementation.

;; If we in fact do have a monad here, it should be the composition of
;; at least one Monad transformer over an identity monad. Possibly
;; also the composition of some other more interesting base monad and
;; transformer.

;; Ugly external interface for cover
;; (define (run . args)
;;   (cond
;;     ((null? (cdr args)) (loop* (car args)))
;;     (else ((loop (car args)) (cadr args)))))

;; (define kons (λ (a) (λ (fk) (cons a (fk)))))
;; (define nill (λ () '()))

;; (define (loop n)
;;   (λ (c)
;;     (if (zero? n)
;;         '()
;;         (((c (loop (sub1 n))) kons) nill))))

;; (define loop*
;;   (λ (c)
;;     (((c loop*) kons) nill)))

