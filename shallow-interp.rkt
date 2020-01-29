#lang racket

;; The first step here is to construct what seems like it should be a
;; simple, monadic interpreter a la Danvy et al. in Racket, with a
;; microKanren-like object language in a putatively monadic Racket
;; metalanguage. We implement this language with our stream-based
;; monadic implementation. We will eschew here the unification aspect;
;; that seems like it would be overkill for what we are trying to
;; expose. However, we will need to ensure that we still have the
;; capacity to carry some state-like value. Danvy et al. also maintain
;; this.

;; Explicitly carrying the state around seems ugly, but as long as it
;; _can_ be curried out, we should not have a problem. Ensure that we
;; *can* always curry it out, and that we have a state, and that we
;; pass it along correctly. Remember Danvy used his programs not for
;; logic programming, but also for Icon-like programming.

;; We define here the language in an interpreter without the monadic
;; abstractions, and then work towards them.

;; We have temporarily fixed the _particular_ sets of relations of the
;; programming language, so that these programs represent queries
;; against _some_ fixed database. Of course, that could have been the
;; nullary database, so conceptually, this language may or may not
;; support the recursion and delay aspects that are the critical
;; difference.

(define (eval g s)
  (match g
    [`(succeed) (list s)]
    [`(fail) (list)]
    [`(^ ,g1 ,g2) ($append-map (eval g1 s) ?????)] ;; (λ (s) (eval g2 s)) or g2
    [`(v ,g1 ,g2) ($append (eval g1 s) (eval g2 s))]
    [`(threeo) (λ () (eval '(twoo) s))]
    [`(twoo)   (λ () (eval '(one) s))]
    [`(oneo)   (λ () (eval '(succeed) s))]
    [`(alwayso) (λ () (eval '(v (succeed) (alwayso)) s))]))

(define ($append $1 $2)
  (cond
    [(null? $1) $2]
    [(procedure? $1) (λ () ($append $2 ($1)))]
    [(pair? $1) (cons (car $1) ($append (cdr $1) $2))]))

(define ($append-map $1 f) ;; rename to g, if it's a goal
  (cond
    [(null? $1) '()]
    [(procedure? $1) (λ () ($append-map $1 f))] 
    [(pair? $1) ($append (?? (car $1) ???) ($append-map (cdr $1) f))])) ;; (f (car $1)) or (eval (car $1) f)

;; [`(^ ,g1 ,g2) ($append-map (eval g1 s) ?????)]

;; This is an interesting line. In the usual, curried version, we can
;; get away with eta-reducing here. It's not clear to me where the
;; (eval g2) should go. It could be that we pass g2 along, and
;; construct the (\(s) (eval g2 s)) inside append-map. Or it could be
;; that we construct at the outside level, and then pass that lambda
;; along. It's unclear which is right. It goes along with this line:

;; [(pair? $1) ($append (?? (car $1) ???) ($append-map (cdr $1) f))]
