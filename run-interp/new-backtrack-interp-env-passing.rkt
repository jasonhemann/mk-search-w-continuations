#lang racket
(require rackunit)
(require racket/trace)
(provide (all-defined-out))

;; If and when this proves insufficiently fast:

;; Turn the relation parameters into peano numbers representing offsets in the relation actual parameters list, look up by recurring on both.

;; Maybe keep the next or current var count, whichever is best, as some kind of offsets from a list index of plus base value.


#| 




Cf. 
Bove & Capretta "Modelling general recursion in type theory" http://www.cs.nott.ac.uk/~pszvc/publications/General_Recursion_MSCS_2005.pdf
Danvy & Hatcliff "CPS-Transformation after Strictness Analysis"
Hatcliff & Danvy "A generic account of Continuation-passing styles" https://dl.acm.org/doi/10.1145/174675.178053
Bieniusa & Thiemann "How to CPS Transform a Monad" - https://link.springer.com/content/pdf/10.1007%2F978-3-642-00722-4_19.pdf 
Danvy "A Unifying Approach to Goal-directed Evaluation" https://link.springer.com/content/pdf/10.1007/BF03037259.pdf
Danvy "A Functional Correspondence Between Evaluators and Abstract Machines" https://dl.acm.org/doi/abs/10.1145/888251.888254
Danvy "Back to Direct Style" - http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=9DF04C851236C5FED9A23F1EE2242BDE?doi=10.1.1.10.7604&rep=rep1&type=pdf
Danvy "Three Steps for the CPS Transformation"
Danvy "On Evaluation Contexts, Continuations, and the Rest of the Computation" https://web.archive.org/web/20201112020559/https://www.cs.bham.ac.uk/~hxt/cw04/danvy.pdf
Lawall & Danvy "Continuation-based Partial Evaluation" https://dl.acm.org/doi/abs/10.1145/182409.182483
Pottier "Revisiting the CPS Transformation and its Implementation"
Hatcliff & Danvy "A Generic Account of continuation-passing styles" - https://dl.acm.org/doi/10.1145/174675.178053
Danvy "A Journey from Interpreters to Compilers and Virtual Machines" - https://www.cc.gatech.edu/GPCE03/gpce-invited.html
Desouter "Towards Flexible Goal-oriented Logic Programming" - https://biblio.ugent.be/publication/7160513/file/7253748
Hillerstrom et al "Continuation Passing Style for Effect Handlers" https://homepages.inf.ed.ac.uk/slindley/papers/handlers-cps-draft-april2017.pdf
Biernacki et al "A Dynamic Continuation-passing Style for Dynamic Delimited Continuations" https://www.brics.dk/RS/06/15/BRICS-RS-06-15.pdf
Wand "Continuation-based Program Transformation Strategies" https://dl.acm.org/doi/pdf/10.1145/322169.322183
Wand & Vallaincourt "Relating Models of Backtracking" https://dl.acm.org/doi/pdf/10.1145/1016850.1016861

|# 

;; Carry around environments

;; Explicitly tag the logic variables and the relation parameters
;; We know that if we see a relation parameter it can contain a logic var inside, from old env.
;; Relation parameters cannot escape

;; I have now removed booleans and numbers from the term language. Yoink!

;; Some fixpoint functions. They're pre-theoretical, really.
(define (fix f)
  (letrec ([g (λ (x) ((f g) x))])
    g))

(define (fix2 f)
  (letrec ([g (λ (x y) ((f g) x y))])
    g))

(define (logic-var n) n)
(define (logic-var? v) (number? v))
(define (same-logic-var? u v)
  (and (number? u)
       (number? v)
       (eqv? u v)))

(define (walk v s)
  (let ([a (and (logic-var? v) (assv v s))])
    (if a (walk (cdr a) s) v)))

(define (apply-param-and-lex-vars* t le pe)
  (match t
    [`(param ,k) (apply-param-env pe k)] ;; since under this assumption, the incoming param cannot contain unevaled logic vars
    [`(var ,x) (apply-lex-env le x)]
    [`(,a . ,d) (cons (apply-param-and-lex-vars* a le pe)
                      (apply-param-and-lex-vars* d le pe))]
    [else t]))

(define (ext-s x v s)
  (cons `(,x . ,v) s))

;; v comes in walked, but not walk*ed
(define (ext-s-check x v s)
  (and (doesnt-occur? x v s) (ext-s x v s)))

;; Does not bring the substitution up to date.
(define (doesnt-occur? x v s)
  (cond
    [(logic-var? v) (not (same-logic-var? v x))] ;; if it occurs, return #t, else return v. 
    [(pair? v)
     (and (doesnt-occur? x (walk (car v) s) s)
          (doesnt-occur? x (walk (cdr v) s) s))]
    [else #t]))

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
    [`(,prog) (loop* (ee prog (init-rel-env) empty-s))]
    [`(,n ,prog) (loop n (ee prog (init-rel-env) empty-s))]))

;; Each as the varname and the expression it's scoped over
;; E.g.

;; '((listo (_ . (disj (conj succeed succeed) (listo))))
;;   (unproductiveo (_ . (unproductiveo))))

(define (init-rel-env)
  (let ([h (make-immutable-hasheqv
            `((a-cat . ,(cons '(t) `(== (param t) cat)))
              (listo . ,(cons '(x) `(disj (conj succeed succeed) (listo (param x)))))
              (nevero . ,(cons '(x) `(nevero)))
              (doggo . ,(cons '(t) `(disj (== (param t) dog) (hoto (param t)))))
              (hoto . ,(cons '(t) `(doggo (param t))))
              (appendo . ,(cons '(t) `(fresh (a)
                                       (fresh (d)
                                         (fresh (res)
                                           (conj (== (param t) ,(cons '(var a) (cons '(var d) '(var res))))
                                                 (disj 
                                                  (conj (== () (var a)) (== (var d) (var res)))
                                                  (fresh (a2)
                                                    (fresh (d2)
                                                      (conj
                                                       (== (var a) ,(cons '(var a2) '(var d2)))
                                                       (fresh (res2)
                                                         (conj (== (var res) ,(cons '(var a2) '(var res2)))
                                                               (appendo ,(cons '(var d2) (cons '(var d) '(var res2))))))))))))))))
              (catso . ,(cons '(t) `(disj (== (param t) cat) (catso (param t)))))
              (outero . ,(cons '(t) `(fresh (a)
                                      (fresh (d)
                                        (fresh (res)
                                          (conj (== (param t) ,(cons '(var a) (cons '(var d) '(var res))))
                                                (fresh (a2)
                                                  (fresh (d2)
                                                    (conj
                                                     (== (var a) ,(cons '(var a2) '(var d2)))
                                                     (fresh (res2)
                                                       (conj (== (var res) ,(cons '(var a2) '(var res2)))
                                                             (innero ,(cons '(var d2) (cons '(var d) '(var res2)))))))))))))))
              (innero . ,(cons '(t) `(fresh (a)
                                      (fresh (d)
                                        (fresh (res)
                                           (conj (== (param t) ,(cons '(var a) (cons '(var d) '(var res))))
                                                (conj (== () (var a)) (== (var d) (var res)))))))))))])
    (λ (y)
      (hash-ref h y (λ () (error 'init-rel-env "sadness ~s~n" y))))))

(define (apply-rel-env env r) (env r))

(define (extend-lexical-env x a env) (cons (cons x a) env))


;; Contemplating: we could use this together with a
;; different representation of the logic variable environment
;; to do var lookup

;; '((e . 5) (d . 4) (c . 3) (b . 2) (a . 1))
;; next var count was 1 when we entered this relation, now its 6

(define (find-var-id x ls nvc)
  (cond
    ((eqv? x (car ls)) nvc)
    (else (find-var-id x (sub1 nvc) (cdr ls)))))

(define (apply-lex-env le t)
  (cdr (assv t le)))


(define (apply-param-env pe t)
  (match pe
    [`((,y . ,ys) . (,v . ,vs))
     (if (eqv? t y)
         v
         (apply-param-env (cons ys vs) t))]))

(define (unify v w s)
  (cond
    [(same-logic-var? v w) s]
    [(logic-var? v) (ext-s-check v w s)]
    [(logic-var? w) (ext-s-check w v s)]
    [(and (pair? v) (pair? w))
     (let ([s (unify (walk (car v) s) (walk (car w) s) s)])
       (and s (unify (walk (cdr v) s) (walk (cdr w) s) s)))]
    [(equal? v w) s]
    [else #f]))

(define (ee expr le pe re s vc)
  (match expr
    [`(succeed)       (return s vc)]
    [`(fail)          (mzero)]
    [`(fresh (,v) ,g) (ee g (extend-lexical-env v vc le) pe re s (add1 vc))]
    [`(== ,t1 ,t2)
     (let ([res (unify (walk (apply-param-and-lex-vars* t1 le pe) s) (walk (apply-param-and-lex-vars* t2 le pe) s) s)])
       (if res
           (return res vc)
           (mzero)))]
    [`(disj ,g1 ,g2) (mplus (ee g1 le pe re s vc) (ee g2 le pe re s vc))]
    [`(conj ,g1 ,g2) (bind (ee g1 le pe re s vc) (λ (s^ vc^) (ee g2 le pe re s^ vc^)))]
    [`(,r . ,expr2s)
     ;; To evaluate a relation, we:
     ;; - evaluate the relname to a relbody and parameter
     ;; - evaluate the expression to a value.
     ;; - start a new environment with a mapping from that parameter to said value.
     (match-let ([`(,params . ,body) (apply-rel-env re r)]
                 [vals (map (λ (e) (apply-param-and-lex-vars* e le pe)) expr2s)])
       (freeze (ee body '() `(,params . ,vals) re s vc)))])) 

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
  (test-equal? "lookup a relation name in an initial env"
    (match-let ([`(,params . ,body) (apply-rel-env (init-rel-env) 'a-cat)])
      params)
    '(t))
  (test-equal? "fail in initial env w/bs initial continuations works"
    (loop* (ee '(fail) '() '() (init-rel-env) '() 0))
    '())
  (test-equal? "success in initial env w/bs initial continuations works"
    (loop* (ee '(succeed) '() '() (init-rel-env) '() 0))
    '(()))
  (test-equal? "fresh x over success in initial env w/bs initial continuations works"
    (loop* (ee '(fresh (x) (succeed)) '() '() (init-rel-env) '() 0))
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

  (test-equal?
    "test walk w/o logic vars or params"
    (walk '0 '((1 . 0)))
    '0)
  
  (test-equal? "fresh x over success in initial env returns correct var count value, too"
    (new-loop* (ee '(fresh (x) (succeed)) '() '() (init-rel-env) '() 0))
    '(() 1))
  (test-equal? "different constants fail to unify, returns as ought"
    (loop* (ee '(== fish dog) '() '() (init-rel-env) '() 0))
    '())
  (test-equal? "same constants unify, obv from the bs init. continuations" 
    (loop* (ee '(== dog dog) '() '() (init-rel-env) '() 0))
    '(()))
  (test-equal? "loop n works, we also get answers out from here" 
    (loop 1 (ee '(== dog dog) '() '() (init-rel-env) '() 0))
    '(()))
  (test-equal? "var name subst works, so unify var and const here works"
    (loop* (ee '(== (var x) dog) '((x . 0)) '() (init-rel-env) '() 1))
    '(((0 . dog))))
  (test-equal? "var name subst works, so unify const and var"
    (loop* (ee '(== dog (var x)) '((x . 0)) '() (init-rel-env) '() 1))
    '(((0 . dog))))
  (test-equal? "var name subst works, so unify const and var"
    (loop* (ee '(== (cat . dog) (var x)) '((x . 0)) '() (init-rel-env) '() 1))
    '(((0 cat . dog))))
  (test-equal? "var name subst works, so fresh + unify works"
    (loop* (ee '(fresh (x) (== (var x) dog)) '() '() (init-rel-env) '() 0))
    '(((0 . dog))))
  (test-equal? "var name subst works, so unification here works"
    (loop* (ee '(fresh (x) (fresh (y) (== (var x) (var y)))) '() '() (init-rel-env) '() 0))
    '(((0 . 1))))
  (test-equal? "var name subst works, so unification here works in opposite order"
    (loop* (ee '(fresh (x) (fresh (y) (== (var y) (var x)))) '() '() (init-rel-env) '() 0))
    '(((1 . 0))))
  (test-equal? "operating w/disj"
    (loop* (ee '(fresh (x) (disj (== (var x) cat) (== (var x) dog))) '() '() (init-rel-env) '() 0))
    '(((0 . cat)) ((0 . dog))))
  (test-equal? "operating w/disj in the opposite order"
    (loop* (ee '(fresh (x) (disj (== (var x) dog) (== (var x) cat))) '() '() (init-rel-env) '() 0))
    '(((0 . dog)) ((0 . cat))))
  (test-equal? "operating w/a basic conj"
    (loop* (ee '(fresh (x) (fresh (y) (conj (== (var x) dog) (== (var y) cat)))) '() '() (init-rel-env) '() 0))
    '(((1 . cat) (0 . dog))))
  (test-equal? "operating w/a basic conj in the opposite order"
    (loop* (ee '(fresh (x) (fresh (y) (conj (== (var y) cat) (== (var x) dog)))) '() '() (init-rel-env) '() 0))
    '(((0 . dog) (1 . cat))))
  (test-equal? "relation call on a non-recursive relation"
    (loop* (ee '(a-cat (var x)) '((x . 0)) '() (init-rel-env) '() 1))
    '(((0 . cat))))
  (test-equal? "relation call on a non-recursive relation w/fresh"
    (loop* (ee '(fresh (x) (a-cat (var x))) '() '() (init-rel-env) '() 0))
    '(((0 . cat))))
  (test-equal? "relation call on half a body of a recursive relation"
    (loop 3 (ee `(catso (param t)) '() `((t) . (0)) (init-rel-env) '() 1))
    '(((0 . cat)) ((0 . cat)) ((0 . cat))))
  (test-equal? "relation call on body of this relation 1 extra ans b/c no delay"
    (loop 3 (ee `(disj (== (param t) cat) (catso (param t))) '() `((t) . (0)) (init-rel-env) '() 1))
    '(((0 . cat)) ((0 . cat)) ((0 . cat)) ((0 . cat))))
  (test-equal? "relation call on a recursive relation"
    (loop 3 (ee `(catso (var x)) '((x . 0)) '() (init-rel-env) '() 1))
    '(((0 . cat)) ((0 . cat)) ((0 . cat))))
  (test-equal? "relation call on a recursive relation w/fresh"
    (loop 3 (ee `(fresh (x) (catso (var x))) '() '() (init-rel-env) '() 0))
    '(((0 . cat)) ((0 . cat)) ((0 . cat))))
  (test-equal? "simple but non-trivial ground relation call w/2 rels on insufficient fuel"
    (loop 1 (ee `(fresh (q) (outero ,(cons (cons 'a '()) (cons '() (cons 'a '()))))) '() '() (init-rel-env) '() 0))
    '())
  (test-equal? "simple but non-trivial ground relation call w/2 rels; two fuels for an answer"
    (loop 5 (ee `(fresh (q) (outero ,(cons (cons 'f '()) (cons '() (cons 'f '()))))) '() '() (init-rel-env) '() 0))
    '(((9) (8) (7) (6) (5) (4 . f) (3 f) (2) (1 f))))
  (test-equal? "simple 2 fuel test shows no inadvertent shadowing b/t constants and variables w/same name"
    (loop 5 (ee `(fresh (q) (outero ,(cons (cons 'a '()) (cons '() (cons 'a '()))))) '() '() (init-rel-env) '() 0))
    '(((9) (8) (7) (6) (5) (4 . a) (3 a) (2) (1 a))))
  (test-equal? "simplest ground relation call on 1-arg appendo"
    (loop 1 (ee `(fresh (q) (appendo ,(cons '() (cons '() '())))) '() '() (init-rel-env) '() 0))
    '(((3) (2) (1))))
  (test-equal? "simplest fresh relation call on 1-arg appendo"
    (loop 1 (ee '(fresh (q) (appendo (var q))) '() '() (init-rel-env) '() 0))
    '(((2 . 3) (1) (0 1 2 . 3))))
  (test-equal? "simplest relation call on 1-arg appendo for two answers"
    (loop 2 (ee '(fresh (q) (appendo (var q))) '() '() (init-rel-env) '() 0))
    '(((2 . 3) (1) (0 1 2 . 3))
      ((8 . 9) (7) (6 . 9) (2 . 8) (5 . 7) (3 4 . 6) (1 4 . 5) (0 1 2 . 3))))
  (test-equal? "relation call on appendo relation"
    (loop 3 (ee `(fresh (q)
        (appendo
         ,(cons (cons 'a (cons 'b '()))
                (cons (cons 'c (cons 'd '())) '(var q)))))
     '()
     '()
     (init-rel-env)
     '()
     0))
    '(((15 c d)
       (12 . 15)
       (14 c d)
       (13)
       (9 10 . 12)
       (11)
       (10 . b)
       (6 . 9)
       (8 c d)
       (7 b)
       (3 4 . 6)
       (5 b)
       (4 . a)
       (0 . 3)
       (2 c d)
       (1 a b))))
  (test-equal? "relation call on a mutually recursive relation"
    (loop 3 (ee '(fresh (q) (doggo (var q))) '() '() (init-rel-env) '() 0))
    '(((0 . dog)) ((0 . dog))))
  (test-equal? "relation call on one of two mut-rec rels; 1 extra fuel doesn't help"
    (loop 4 (ee '(fresh (q) (doggo (var q))) '() '() (init-rel-env) '() 0))
    '(((0 . dog)) ((0 . dog)))))


