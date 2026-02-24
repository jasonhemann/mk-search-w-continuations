#lang racket
(provide (all-defined-out))

;; To do for next week:
;; 1. Try and paint the narrative about how/what you're trying to get from Danvy et al.
;; Give the narrative version of the story about what you're trying to do by following their research.
;; What does the compiling with continuations get you?
;; 

(define (var x) x)
(define (var? x) (number? x))

;; Independent of control flow and backtracking operators; awesome.
(define ((call/fresh f) s/c)
  (let ((c (cdr s/c)))
    ((f (var c)) `(,(car s/c) . ,(+ c 1)))))

(define (unify u v s)
  (cond
    ((eqv? u v) s)
    ((var? u) (ext-s u v s))
    ((var? v) (unify v u s))
    ((and (pair? u) (pair? v))
     (let ((s (unify (find (car u) s) (find (car v) s) s)))
       (and s (unify (find (cdr u) s) (find (cdr v) s) s))))
    (else #f)))

(define (find u s) 
  (let ((pr (and (var? u) (assv u s))))
    (if pr (find (cdr pr) s) u)))

(define (ext-s x u s)
  (cond
    ((occurs? x u s) #f) 
    (else `((,x . ,u) . ,s))))

(define (occurs? x u s)
  (cond
    ((var? u) (eqv? x u))
    ((pair? u) (or (occurs? x (find (car u) s) s)
                   (occurs? x (find (cdr u) s) s)))
    (else #f)))

;; F Ans = (-> Ans)
;; S Ans = (St Comp -> Ans)
;; D Ans = ((-> Comp) -> Ans)

;; A DT like (cons 'Nil eeee)
;; data Stream = Nil | Cons x Stream | Delay (-> Stream)

;; Comp = (Ans) ((F Ans) (S Ans) (D Ans) -> Ans)


;; -- Wrong 
;; Comp Ans = ((F Ans) (S Ans) (D Ans)-> Ans)
;; codata Comp fk sk dk = Nil | Cons x Comp | Comp 

;; 1. Good reason why Delay, etc,---why I wanted a straightforward
;; datatype

;; 2. Iteratively by CPSing.

;; 2a. How would you cps all 3 posibilities, what's the order that you
;; did them in? "There's no operation that can continue without one of
;; them."

;; These operations look the same no matter their representation:
;; Making it RI means we have an ADT, monad-ish thing against which to
;; compare cross-implementations

(define ((disj g1 g2) s/c) (disj-help (g1 s/c) (g2 s/c)))
(define ((conj g1 g2) s/c) ($append-map (g1 s/c) g2))

(define-syntax-rule (define-relation (defname . args) g)
  (define (((defname . args) s/c) fk dk sk)
    ;; (delay/name
    ;;  ((g s/c) ;; b/c this is now a computation
    ;;   fk
    ;;   dk
    ;;   sk))

    ;; but now makes more sense to me to have this /be/ the delay
    ;; operation, right? 
    
    (dk (g s/c)
        fk ;;?
        ;;dk? 
        sk ;;?
        
        )
    ))

($app ($app (== ...) (== ...)))

;; (Comp x Comp -> Comp) 
(define ($append $1 $2)
  (cond
    ((null? $1) $2)
    ((promise? $1) (delay/name ($append $2 (force $1))))
    (else (cons (car $1) ($append (cdr $1) $2)))))

;; ((Comp Ans) x (Comp Ans) -> (Comp Ans)) 
(define ($append $1 $2)
  ($1 (lambda () $2))
  (cond
    ((null? $1) $2)
    ((promise? $1) (delay/name ($append $2 (force $1))))
    (else (cons (car $1) ($append (cdr $1) $2)))))

;; AKA append$ 
(define ((disj-help c1 c2) fk dk sk)
  (c1
   (lambda () (c2 fk dk sk))
   (lambda (c^)
     (delay/name ;; I still need to delay/name? no? unclear.
      ((disj-help c2 c^)
       ;; fk ? 
       ;; dk ? 
       ;; sk ? 
       )))
   sk))

#| Goal × Stream → Stream |#
;; (define ($append-map g $)
;;   (cond
;;     ((null? $) '())
;;     ((promise? $) (delay/name ($append-map g (force $))))
;;     (else ($append (g (car $)) ($append-map g (cdr $))))))

;; Goal x (Comp Ans) ->  (Comp Ans)
(define ($append-map g $)
  ($ (lambda ()
       (lambda (fk dk sk) (fk)))
     (lambda ($^)
       (lambda (fk dk sk)
         (dk (delay/name ($append-map g (force $^))))))
     (lambda (a res)
       ($append (g a) ($append-map g res)))))

;; (define (($append-map c g) fk dk sk)
;;   (c fk
;;      (lambda (c^) (delay/name (($append-map (force c) g)
;;                                ;; fk
;;                                ;; dk
;;                                ;; sk
;;                                )))
;;      (lambda (a res)
;;        ((disj-help (g a) ($append-map res g))
;;         fk 
;;         dk
;;         sk))))

;; (define ((== u v) s/c)
;;   (let ((s (car s/c)))
;;     (let ((s (unify (find u s) (find v s) s)))
;;       (if s (list `(,s . ,(cdr s/c))) `()))))

(define ((== u v) s/c) ;; Yes these can be sunk into this expression
  (let ((s (car s/c)))
    (let ((s (unify (find u s) (find v s) s)))
      (if s
          (lambda (fk dk sk) 
            (sk `(,s . ,(cdr s/c)) fk)) ;; what had been (list ,new-state)
          (lambda (fk dk sk) 
            (fk))))))           ;; what had been `() 


(define ((== u v) s/c) ;; Yes these can be sunk into this expression
  (lambda (fk dk sk) 
    (let ((s (car s/c)))
      (let ((s (unify (find u s) (find v s) s)))
        (if s
            (sk `(,s . ,(cdr s/c)) fk) ;; what had been (list ,new-state)
            (fk))))))
;; a "run" to correspond w/it. 
(define (call/initial-state n g)
  (take2 n (g '(() . 0))))

;; Our last-time implementation

;; Nat x (Comp Ans) -> Ans
(define (take2 n $)
  (if (zero? n)
      '()
      ;; It surprises me that these would be correct, now, upon thinking about it. 
      ($ (lambda () '())
         (lambda ($2) (take2 n (force $2)))
         (lambda (a res) (cons a (take2 (sub1 n) res))))))


;; Why our old take and pull was different: I'd changed the meaning of
;; `n`; not the number of answers, but the (max) number of times to
;; force the immature tail stream. This I thought seemed a simpler
;; partial-computation behavior than the # of anwers, and so a way to
;; simplify the problem down some.

;; (define (take n $)
;;   (cond
;;     ((null? $) '())
;;     ((and n (zero? (- n 1))) (list (pull (car $))))
;;     (else (cons (car $) 
;;             (take (and n (- n 1)) (pull (cdr $)))))))

;; (define (pull $) (if (promise? $) (pull (force $)) $))




;; Our Data-driven model of $n$ answers from last time.
;; (define (take n $)
;;   (if (zero? n)
;;       '()
;;       (cond
;;         ((null? $) '())
;;         ((promise? $) (take n (force $)))
;;         ((cons? $) (cons (car $) (take (sub1 n) (cdr $)))))))

;; (take n (pull (g '(() . 0))))

;; HUKARZ
;; (define ((ifte g0 g1 g2) s/c)
;;   (let loop (($ (g0 s/c)))
;;     (cond
;;       ((null? $) (g2 s/c))
;;       ((promise? $) (delay/name (loop (force $))))
;;       (else ($append-map g1 $)))))

;; (define ((once g) s/c)
;;   (let loop (($ (g s/c)))
;;     (cond
;;       ((null? $) '())
;;       ((promise? $) (delay/name (loop (force $))))
;;       (else (list (car $))))))

;; (define (var x) x)
;; (define (var? x) (number? x))
;; (define (deref u s) 
;;   (let ((pr (and (var? u) (assv u s))))
;;     (if pr (deref (cdr pr) s) u)))
;; (define (ext-s x u s)
;;   (cond
;;     ((occurs? x u s) #f) 
;;     (else `((,x . ,u) . ,s))))
;; (define (occurs? x u s)
;;   (let ((u (deref u s)))
;;     (cond
;;       ((var? u) (eqv? x u))
;;       ((pair? u) (or (occurs? x (car u) s)
;; 		     (occurs? x (cdr u) s)))
;;       (else #f))))
;; (define (unify u v s)
;;   (let ((u (deref u s)) (v (deref v s)))
;;     (cond
;;       ((eqv? u v) s)
;;       ((var? u) (ext-s u v s))
;;       ((var? v) (unify v u s))
;;       ((and (pair? u) (pair? v))
;;        (let ((s (unify (car u) (car v) s)))
;; 	 (and s (unify (cdr u) (cdr v) s))))
;;       (else #f))))
;; (define ((== u v) s/c)
;;   (let ((s (unify u v (car s/c))))
;;     (if s (list (cons s (cdr s/c))) `())))
;; (define (pull $) (if (promise? $) (pull (force $)) $))
;; (define (call/initial-state n g) 
;;   (take n (pull (g '(() . 0)))))
;; (define ((call/fresh f) s/c)
;;   (let ((c (cdr s/c)))
;;     ((f (var c)) (cons (car s/c) (+ c 1)))))
;; (define ((disj g1 g2) s/c) ($append (g1 s/c) (g2 s/c)))
;; (define ((conj g1 g2) s/c) ($append-map (g1 s/c) g2))
;; (define ($append $1 $2)
;;   (cond
;;     ((null? $1) $2)
;;     ((promise? $1) (delay/name ($append $2 (force $1))))
;;     (else (cons (car $1) ($append (cdr $1) $2)))))
;; (define ($append-map $ g)
;;   (cond
;;     ((null? $) `())
;;     ((promise? $) (delay/name ($append-map (force $) g)))
;;     (else ($append (g (car $)) ($append-map (cdr $) g)))))
;; (define-syntax-rule (define-relation (defname . args) ge)
;;   (define ((defname . args) s/c) (delay/name (ge s/c))))
;; (define (take n $)
;;   (cond
;;     ((null? $) '())
;;     ((and n (zero? (- n 1))) `(,(car $)))
;;     (else (cons (car $) 
;;             (take (and n (- n 1)) (pull (cdr $)))))))
;; (define ((ifte g0 g1 g) s/c)
;;   (let loop (($ (g0 s/c)))
;;     (cond
;;       ((null? $) (g s/c))
;;       ((promise? $) (delay/name (loop (force $))))
;;       (else ($append-map $ g1)))))
;; (define ((once g) s/c)
;;   (let loop (($ (g s/c)))
;;     (cond
;;       ((null? $) '())
;;       ((promise? $) (delay/name (loop (force $))))
;;       (else (list (car $))))))

#| 
#lang racket
;; microKanren DLS '16
;; Jason Hemann, Dan Friedman, Will Byrd, & Matt Might

#| Nat → Var |#
(define (var n) n)

#| Term → Bool |#
(define (var? t) (number? t))

#| Term × Subst → Term |#
(define (find u s) 
  (let ((pr (and (var? u) (assv u s))))
    (if pr (find (cdr pr) s) u)))

#| Var × Term × Subst → Maybe Subst |#
(define (ext-s x v s)
  (cond 
    ((occurs? x v s) #f)
    (else (cons `(,x . ,v) s))))

#| Var × Term × Subst → Bool |#
(define (occurs? x u s)
  (cond
    ((var? u) (eqv? x u))
    ((pair? u) (or (occurs? x (find (car u) s) s)
                   (occurs? x (find (cdr u) s) s)))
    (else #f)))

#| Term × Term × Subst → Maybe Subst |#
(define (unify u v s)
  (cond
    ((eqv? u v) s)
    ((var? u) (ext-s u v s))
    ((var? v) (unify v u s))
    ((and (pair? u) (pair? v))
     (let ((s (unify (find (car u) s) (find (car v) s) s)))
       (and s (unify (find (cdr u) s) (find (cdr v) s) s))))
    (else #f)))

#| Term × Term → Goal |#
(define ((== u v) s/c)
  (let ((s (car s/c)))
    (let ((s (unify (find u s) (find v s) s)))
      (if s (list `(,s . ,(cdr s/c))) `()))))

((== '#t 'z) '(() . 0))
'()
((== '#t '#t) '(() . 0))
'((() . 0))
((== '(#t . #f) '(#t . #f)) '(() . 0))
'((() . 0))

#| (Var → Goal) → Goal |#
(define ((call/fresh f) s/c)
  (let ((c (cdr s/c)))
    ((f (var c)) `(,(car s/c) . ,(+ c 1)))))

((call/fresh
    (λ (x) 
       (== x 'a)))
   '(() . 0))
'((((0 . a)) . 1))

(define-syntax-rule (define-relation (defname . args) g)
  (define ((defname . args) s/c) (delay/name (g s/c))))

(define-relation (append l s o)
  (conde
    ((== '() l) (== s o))
    ((fresh (a d)
       (== `(,a . ,d) l)
       (fresh (r)
         (== `(,a . ,r) o)
         (append d s r))))))

(define-relation (peano n)
  (disj
    (== n 'z)
    (call/fresh 
      (λ (r)
        (conj
          (== n `(s ,r))
          (peano r))))))

(define-relation (unproductive n)
  (unproductive n))

(define ($append $1 $2)
  (cond
    ((null? $1) $2)
    ((promise? $1) (delay/name ($append $2 (force $1))))
    (else (cons (car $1) ($append (cdr $1) $2)))))

#| Goal × Stream → Stream |#
(define ($append-map g $)
  (cond
    ((null? $) '())
    ((promise? $) (delay/name ($append-map g (force $))))
    (else ($append (g (car $)) ($append-map g (cdr $))))))

#| Goal × Goal → Goal |#
(define ((disj g1 g2) s/c) ($append (g1 s/c) (g2 s/c)))

#| Goal × Goal → Goal |#
(define ((conj g1 g2) s/c) ($append-map g2 (g1 s/c)))
 ((disj 
     (call/fresh 
       (λ (x) 
         (== 'z x)))
     (call/fresh 
       (λ (x) 
         (== '(s z) x))))
   '(() . 0))
'((((0 . z)) . 1) (((0 . (s z))) . 1))

((call/fresh 
   (λ (x) 
     (call/fresh
       (λ (y) 
         (conj 
           (== y x)
           (== 'z x))))))
 '(() . 0))
'((((0 . z) (1 . 0)) . 2))


((call/fresh
   (λ (n)
     (peano n)))
 '(() . 0))

#| Maybe Nat⁺ × Goal→ Mature |#
(define (call/initial-state n g) 
  (take n (pull (g '(() . 0)))))

#| Stream → Mature |#
(define (pull $) (if (promise? $) (pull (force $)) $))

#| Maybe Nat⁺ × Mature → List |#
(define (take n $)
  (cond
    ((null? $) '())
    ((and n (zero? (- n 1))) (list (car $)))
    (else (cons (car $) 
            (take (and n (- n 1)) (pull (cdr $)))))))

(call/initial-state 2
    (call/fresh
      (λ (n)
        (peano n))))
'((((0 . z)) . 1) (((1 . z) (0 . (s 1))) . 2))

(call/initial-state 1
  (call/fresh
    (λ (n)
      (disj 
        (unproductive n)
        (peano n)))))

(define-relation (church n)
  (call/fresh 
    (λ (b)
      (conj
        (== n `(λ (s) (λ (z) ,b)))
        (peano b)))))

 (call/initial-state 3
    (call/fresh
      (λ (n)
       (disj
         (peano n)
         (church n)))))
'((((0 . z)) . 1)
  (((1 . z) (0 . (s 1))) . 2)
  (((1 . z) (0 . (λ (s) (λ (z) 1)))) . 2))

(define (desugared-append l s o)
  (λ (s/c)
    (delay/name
      ((disj
         (conj 
           (== l '())
           (== s o))
         (call/fresh
           (λ (a)
             (call/fresh 
               (λ (d)
                 (conj 
                   (== l `(,a . ,d))
                   (call/fresh 
                     (λ (r)
                       (conj 
                         (== o `(,a . ,r))
                         (desugared-append d s r))))))))))
       s/c)))) 

 (call/initial-state #f
    (call/fresh
      (λ (q)
        (desugared-append '(t u v) '(w x) q))))

(define ((ifte g0 g1 g2) s/c)
  (let loop (($ (g0 s/c)))
    (cond
      ((null? $) (g2 s/c))
      ((promise? $) (delay/name (loop (force $))))
      (else ($append-map $ g1)))))

(call/initial-state #f
   (call/fresh
     (λ (q)
       (ifte (== 'a 'b) (== q 'a) (== q 'b)))))
'((((0 . b)) . 1))

(define ((once g) s/c)
  (let loop (($ (g s/c)))
    (cond
      ((null? $) '())
      ((promise? $) (delay/name (loop (force $))))
      (else (list (car $))))))

(call/initial-state #f
   (call/fresh
     (λ (q)
       (once (peano q)))))
'((((0 . z)) . 1))

(define-syntax disj+
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (disj g0 (disj+ g ...)))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (conj g0 (conj+ g ...)))))

(define-syntax-rule (conde (g0 g ...) (g0* g* ...) ...)
  (disj+ (conj+ g0 g ...) (conj+ g0* g* ...) ...))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh (λ (x0) (fresh (x ...) g0 g ...))))))

(define-syntax ifte*
  (syntax-rules ()
    ((_ g) g)
    ((_ (g0 g1) (g0* g1*) ... g)
     (ifte g0 g1 (ifte* (g0* g1*) ... g)))))

(define-syntax-rule (conda (g0 g1 g ...) ... (gn0 gn ...))
  (ifte* (g0 (conj+ g1 g ...)) ... (conj+ gn0 gn ...)))

(define-syntax-rule (condu (g0 g1 g ...) ... (gn0 gn ...))
  (conda ((once g0) g ...) ... ((once gn0) gn ...)))

(define (apply-subst v s)
  (let ((v (find v s)))
    (cond
      ((var? v) v)
      ((pair? v) (cons (apply-subst (car v) s)
                       (apply-subst (cdr v) s)))
      (else v))))

(define (build-r v s c)
  (cond
    ((var? v) `((,v . ,(+ (length s) c)) . ,s))
    ((pair? v) (build-r (cdr v) (build-r (car v) s c) c))
    (else s)))

(define (project-var0 s/c)
  (let ((v (apply-subst (var 0) (car s/c))))
    (let ((v (apply-subst v (build-r v '() (cdr s/c)))))
      (apply-subst v (build-r v '() 0)))))

(define-syntax-rule (run n (q) g0 g ...)
  (map project-var0 
    (call/initial-state n (fresh (q) g0 g ...))))

(map project-var0
   (call/initial-state #f
     (call/fresh
       (λ (q)
         (call/fresh
           (λ (l)
             (call/fresh
               (λ (s)
                 (conj
                   (== `(,l ,s) q) 
                   (append l s '(t u v w x)))))))))))

(run #f (q) (append '(t u v) '(w x) q))
'((t u v w x))

(run #f (q) (append '(t u v) q '(t u v w x)))
'((w x))

(run #f (q) (fresh (l s) (== `(,l ,s) q) (append l s '(t u v w x))))
|# 
