#lang racket
(require rackunit)
(require racket/trace)
(provide (all-defined-out))

;; Some fixpoint functions. They're pre-theoretical, really.
(define (fix f)
  (letrec ([g (λ (x) ((f g) x))])
    g))

(define (fix2 f)
  (letrec ([g (λ (x y) ((f g) x y))])
    g))

(define (var n) n)
(define (var? v) (number? v))

(define (walk v s)
  (let ([a (assv v s)])
    (if a (walk (cdr a) s) v)))

(define (ext-s x v s) (cons `(,x . ,v) s))

(define (ext-s-check x v s)
  (cond
    [(occurs-check x v s) #f]
    [else (ext-s x v s)]))

(define (occurs-check x v s)
  (cond
    [(var? v) (eqv? v x)]
    [(pair? v)
     (or (occurs-check x (walk (car v) s) s)
         (occurs-check x (walk (cdr v) s) s))]
    [else #f]))

(define (unify v w s)
  (cond
    [(eqv? v w) s]
    [(var? v) (ext-s-check v w s)]
    [(var? w) (ext-s-check w v s)]
    [(and (pair? v) (pair? w))
     (let ((s (unify (walk (car v) s) (walk (car w) s) s)))
       (and s (unify (walk (cdr v) s) (walk (cdr w) s) s)))]
    [(equal? v w) s]
    [else #f]))

(define empty-s '())
(define init-vc 0)

(define loop
  (fix2 
    (λ (loop)
      (λ (n c)
        (c
         (λ (c^)
           (if (zero? n)
               '()
               (loop (sub1 n) c^)))
         (λ (a vc c)
           (cons a (loop n c)))
         (λ ()
           '()))))))

(define loop*
  (fix 
    (λ (loop*)
      (λ (c)
        (c
         (λ (c^)
           (loop* c^))
         (λ (a vc c)
           (cons a (loop* c)))
         (λ ()
           '()))))))

(define (run . args)
  (match args
    [`(,prog) (loop* (ee prog init-env empty-s))]
    [`(,n ,prog) (loop n (ee prog init-env empty-s))]))

;; Each as the varname and the expression it's scoped over
;; E.g.
(define init-env
  '((listo (_ . (disj (conj succeed succeed) (listo))))
    (unproductiveo (_ . (unproductiveo)))))

(define (ae r env)   (cadr (assv r env)))
(define (xe x a env) (cons (cons x a) env))

;; Presently, since we have kludged the logic var environment and the
;; relation environment into one, if some goofball uses a relation
;; name as a local variable, instead of an unbound identifier, we'll
;; drop in the relation definition

(define (et t le/re)
  (match t
    [`(var ,x) (let ([pr (assv x le/re)])
                 (if pr (cdr pr) x))]  ;; We switch to internal rep here
    [`(cons ,t1 ,t2) `(,(et t1 le/re) . ,(et t2 le/re))]
    [constant constant]))

(define (subst-arg v newe term)
  (match term
    [`(var ,x)        `(var ,x)]
    [`(cons ,t1 ,t2) `(cons ,(subst-t v newe t1) ,(subst-t v newe t2))]
    [constant         (if (eqv? constant v) newe term)]))

(define (subst-t v newe term)
  (match term
    [`(var ,x)        (if (eqv? x v) newe term)]
    [`(cons ,t1 ,t2) `(cons ,(subst-t v newe t1) ,(subst-t v newe t2))]
    [constant         constant]))

(define (subst/arg v newe in)
  (match in
    [`(succeed)        `(succeed)]
    [`(fail)           `(fail)]
    [`(fresh (,x) ,g)
     (if (eqv? x v)    `(fresh (,x) ,g)
                       `(fresh (,x) ,(subst/arg v newe g)))]
    [`(== ,t1 ,t2)     `(== ,(subst-arg v newe t1) ,(subst-arg v newe t2))]
    [`(disj ,g1 ,g2)   `(disj ,(subst/arg v newe g1) ,(subst/arg v newe g2))]
    [`(conj ,g1 ,g2)   `(conj ,(subst/arg v newe g1) ,(subst/arg v newe g2))]
    [`(,r ,expr2)      `(,r ,(subst-arg v newe expr2))]))

(define (subst/var v newe in)
  (match in
    [`(succeed)        `(succeed)]
    [`(fail)           `(fail)]
    [`(fresh (,x) ,g)
     (if (eqv? x v)    `(fresh (,x) ,g)
                       `(fresh (,x) ,(subst/var v newe g)))]
    [`(== ,t1 ,t2)     `(== ,(subst-t v newe t1) ,(subst-t v newe t2))]
    [`(disj ,g1 ,g2)   `(disj ,(subst/var v newe g1) ,(subst/var v newe g2))]
    [`(conj ,g1 ,g2)   `(conj ,(subst/var v newe g1) ,(subst/var v newe g2))]
    [`(,r ,expr2)      `(,r ,(subst-t v newe expr2))]))

;; Effectively permits a lisp2-style environment and environment lookup.

(define (ee expr le/re s vc)
  (match expr
    [`(succeed) (return s vc)]
    [`(fail) (mzero)]
    [`(fresh (,v) ,g) (ee g (xe v vc le/re) s (add1 vc))]
    [`(disj ,g1 ,g2) (mplus (ee g1 le/re s vc) (ee g2 le/re s vc))]
    [`(conj ,g1 ,g2) (bind (ee g1 le/re s vc) (lambda (s^ vc^) (ee g2 le/re s^ vc^)))]
    [`(,r ,expr2)
     (freeze (ee (subst/arg (car (ae r le/re)) expr2 (cdr (ae r le/re))) le/re s vc))]))

(define-syntax-rule (freeze e) 
  (λ (dk sk fk)
    (dk e)))

(define (return a vc)
  (λ (dk sk fk)
    (sk a vc (mzero))))
  
(define bind
  (fix2
    (λ (bind)
      (λ (m f)
        (λ (dk sk fk)
          (m
           (λ (m^)
             (dk (bind m^ f)))
           (λ (b vc c)
             ((mplus (f b vc) (bind c f))
              dk
              sk
              fk))
           fk))))))

(define (mzero)
  (λ (dk sk fk)
    (fk)))

;; Assume you took the continuations success, delay, fail.
;; And you packaged state := ⟨sub × var-count⟩
;; then the functions are ordered 1 arg, 2 arg, 3 arg, 2 arg, 1 arg, 0 arg.
(define mplus
  (fix2 
   (λ (mplus)
     (λ (m1 m2)
       (λ (dk sk fk)
         (m1
          (λ (c1^)
            (dk (mplus m2 c1^)))
          (λ (a vc c)
            (sk a vc (mplus c m2)))
          (λ ()
            (m2 dk sk fk))))))))

(module+ test
  (test-equal? "contant terms eval properly in empty env"
    (et 'cat '())
    'cat)
  (test-equal? "evaluating a variable in an environment produces its value--a run-time var representation"
    (et '(var x) '((x . 0)))
    '0)
  (test-equal? "fail in initial env w/bs initial continuations works"
    (loop* (ee '(fail) '() '() 0))
    '())
  (test-equal? "success in initial env w/bs initial continuations works"
    (loop* (ee '(succeed) '() '() 0))
    '(()))
  (test-equal? "fresh x over success in initial env w/bs initial continuations works"
    (loop* (ee '(fresh (x) (succeed)) '() '() 0))
    '(()))
  (define new-loop*
    (fix 
     (λ (new-loop*)
       (λ (c)
         (c
          (λ (c^)
            (new-loop* c^))
          (λ (a vc c)
            (cons a (cons vc (new-loop* c))))
          (λ ()
            '()))))))  
  (test-equal? "fresh x over success in initial env returns correct var count value, too"
    (new-loop* (ee '(fresh (x) (succeed)) '() '() 0))
    '(() 1)))

