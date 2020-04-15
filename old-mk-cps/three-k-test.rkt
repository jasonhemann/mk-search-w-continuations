#lang racket
(require racket/trace)

(define kons (curry (λ (a b c) (cons a (c)))))
(define nill (thunk '()))
;; use looper to kick it off
;; or identity to run one at a time. 

;; jstr a = End | More a (jstr a) | Later of () -> (jstr a) 

;; it may be I'm creating a fusion of join and map here,
;; and using them both together,
;; and that that's unfortunate.

;; We could have fks take a k and stick that at the end of the to-do list. 

;; Suppose just that we wanted to be able to halt and continue. Sk/fk system where we could delay at some point.
;; We'd need to be able to hand back a computation.
;; should this just be the empty-k?
;; We can either hard-code the constant, or pass it through.
;; hard-coding the constant means we don't need to thread it. 
;; This needing to pass along something that actually /eats/ an fk reminds me of mitch's * operator. 
;; ah, but (ek init-k) isn't a /computation/. I suppose I could make it return one?
;; Maybe pass a flag that will at one point make it a computation and at the other point run it like an error k?
;; To make it a computation, all we need to do is wrap it in a thunk that'll say what to do when we succeed, delay, or fail.
;; e.g. T F combinators.

(define (unit a)
  (λ (sk)
    (λ (fk)
      ((sk a) fk))))

;; (bind m f) = (join (map f m))

;; bind = join o map

;; So, what does an f look like?
;; a -> b
;; add1, e.g.

;; whereas for bind it looks like a -> M b

;; unit sk fk

(define (bind m f)
  (λ (sk)
    (λ (fk)
      ((c (λ (b)
            (((f b)
              sk)
             fk)))
       fk))))

(define (map c f)
  (λ (sk)
    (λ (fk)
      ((c (λ (a)
            (λ (fk)
              (cons (f a) (fk))))) ;; maybe λ () around that invocation? Or just not invoke it? 
       (fk)))))

;; (define (join cc)
;;   )


(define ($map $1 f)
  (match $1
    [(end) $1]
    [(later $k) (later (λ () ($map ($k) f)))] ;; this looks like invoking an fk. 
    [(cons u $3) (cons (f u) ($map f $3))]))

;; $join :: MMα → Mα 
(define ($join $$1)
  (match $$1
    [(end) $$1]
    [(later $k) $$1]
    [(cons $1 $$3) ($append $1 ($join $$3))]))

;; $append :: Mα → Mα → Mα 
(define ($append $1 $2)
  (match $1
    [(end) $2]
    [(later $k) (later (λ () ($append ($k) $2)))] ;; The interleave or not. 
    [(cons u $3) (cons u ($append $3 $2))]))



(define (bind c f)
  (λ (mk)
    (λ (lk)
      (λ (ek)
        (((c (λ (a)
               ;; probably don't take in an lk here
               ;; it has extra work we don't wanna do. 
               (λ (ek)
                 ((((f a) mk)
                   (λ (c) 
                     (lk (disj (ek init-k) c))))
                  ek))))
          (λ (c)
            (lk (bind c f))))
         ek)))))

(define (disj c1 c2)
  (λ (mk) ;; more
    (λ (lk) ;; later
      (λ (ek) ;; end
        (((c1 mk)
          (λ (c) 
            (lk (disj c2 c))))
         (λ (c3)
           (((c2 mk) lk) (ek c3))))))))

(define (unit a)
  (λ (mk)
    (λ (lk)
      (λ (ek)
        ;; should this take an lk? 
        ((mk a)
;;         (      lk)
;;         don't pass an lk back, we don't want to take it. 
         ek)))))

(define S 
  (unit '#t))

(define F
  (λ (mk)
    (λ (lk)
      (λ (ek)
        (ek init-k)))))

(define (allo x)
  (λ (mk)
    (λ (lk)
      (λ (ek)
        (lk (disj S (allo x)))))))

(define (nats n)
  (λ (mk)
    (λ (lk)
      (λ (ek)
        (lk (disj (unit n) (nats (add1 n))))))))

(define (nat1 n)
  (λ (mk)
    (λ (lk)
      (λ (ek)
        (lk (disj (unit n) (nat2 (add1 n))))))))

(define (nat2 n)
  (λ (mk)
    (λ (lk)
      (λ (ek)
        (lk (disj (unit n) (nat3 (add1 n))))))))

(trace-define (nat3 n)
  (λ (mk)
    (trace-lambda #:name running (lk)
      (trace-lambda #:name innermost (ek)
        (lk (unit n))))))


(define (moreo x)
  (λ (mk)
    (λ (lk)
      (λ (ek)
        (lk (bind S moreo))))))

(define looper
  (λ (x)
    (((x kons) looper) nill)))

;; Welcome to Racket v6.8.
;; ﻿> 
;; three-k-test.rkt﻿> (disj S (disj S F))
;; #<procedure:...three-k-test.rkt:18:2>
;; three-k-test.rkt﻿> (disj S)
;; ; disj: arity mismatch;
;; ;  the expected number of arguments does not match the given number
;; ;   expected: 2
;; ;   given: 1
;; ;   arguments...:
;; ;    #<procedure:S>
;; three-k-test.rkt﻿> S
;; #<procedure:S>
;; three-k-test.rkt﻿> ((S cons) null)
;; #<procedure:...three-k-test.rkt:30:6>
;; three-k-test.rkt﻿> (((S cons) null) null)
;; ; cons: arity mismatch;
;; ;  the expected number of arguments does not match the given number
;; ;   expected: 2
;; ;   given: 1
;; ;   arguments...:
;; ;    #t
;; three-k-test.rkt﻿> (((S (curry cons)) null) null)
;; #<procedure:curried>
;; three-k-test.rkt﻿> (S (curry cons))
;; #<procedure:...three-k-test.rkt:29:4>
;; three-k-test.rkt﻿> ((S (curry cons)) '_)
;; #<procedure:...three-k-test.rkt:30:6>
;; three-k-test.rkt﻿> (((S (curry cons)) '_) '_)
;; #<procedure:curried>
;; three-k-test.rkt﻿> ((((S (curry cons)) '_) '_) '_)
;; '(#t . _)
;; three-k-test.rkt﻿> ((((S (λ (x) (λ (y) (cons x y)))) '_) '_) '_)
;; '(#t . _)
;; three-k-test.rkt﻿> 
;; ; unit: undefined;
;; ;  cannot reference an identifier before its definition
;; ;   in module: "/Users/jhemann/311/three-k-test.rkt"
;; ; Context:
;; ;  /Users/jhemann/311/three-k-test.rkt:1:1 [running body]

;; ﻿> 
;; three-k-test.rkt﻿> (S (curry cons))
;; #<procedure:...three-k-test.rkt:29:4>
;; three-k-test.rkt﻿> ((S (curry cons)) '_)
;; #<procedure:...three-k-test.rkt:30:6>
;; three-k-test.rkt﻿> (((S (curry cons)) '_) '_)
;; ; application: not a procedure;
;; ;  expected a procedure that can be applied to arguments
;; ;   given: '(#t . _)
;; ;   arguments...:
;; ;    '_
;; three-k-test.rkt﻿> 
;; three-k-test.rkt﻿> (((S (curry cons)) '_) '_)
;; '(#t . _)
;; three-k-test.rkt﻿> (((S (curry cons)) '_) '())
;; '(#t)
;; three-k-test.rkt﻿> (((S (curry cons)) '_) '())
;; '(#t)
;; three-k-test.rkt﻿> ((((disj S (disj S F)) (curry cons)) '_) '())
;; '(#t . #<procedure:...three-k-test.rkt:23:9>)
;; three-k-test.rkt﻿> (define kons (λ (a) (λ (b) (cons a (b)))))
;; three-k-test.rkt﻿> (define nill (λ () (list)))
;; three-k-test.rkt﻿> (((S kons) '_) nill)
;; '(#t)
;; three-k-test.rkt﻿> ((((disj S (disj S F)) kons) '_) nill)
;; '(#t #t)
;; three-k-test.rkt﻿> ((((disj F (disj S F)) kons) '_) nill)
;; '(#t)
;; three-k-test.rkt﻿> ((((disj S (disj F F)) kons) '_) nill)
;; '(#t)
;; three-k-test.rkt﻿> ((((disj F (allo)) kons) (λ (x) x)) nill)
;; #<procedure:...three-k-test.rkt:17:2>
;; three-k-test.rkt﻿> (((((disj F (allo)) kons) (λ (x) x)) nill) (λ (x) x))
;; #<procedure:...three-k-test.rkt:18:4>
;; three-k-test.rkt﻿> ((((((disj F (allo)) kons) (λ (x) x)) nill) (λ (x) x)) (λ (x) x))
;; #<procedure:...three-k-test.rkt:19:6>
;; three-k-test.rkt﻿> (((((((disj F (allo)) kons) (λ (x) x)) nill) (λ (x) x)) (λ (x) x)) (λ (x) x))
;; ; application: not a procedure;
;; ;  expected a procedure that can be applied to arguments
;; ;   given: #t
;; ;   arguments...:
;; ;    #<procedure:...three-k-test.rkt:23:9>
;; three-k-test.rkt﻿> 5
;; 5
;; 5
;; three-k-test.rkt﻿> (((((((disj F (allo)) kons) (λ (x) x)) nill) kons) identity) nill)
;; '(#t . #<procedure:...three-k-test.rkt:17:2>)
;; three-k-test.rkt﻿> (((((((disj F (allo)) kons) identity) nill) kons) identity) nill)
;; '(#t . #<procedure:...three-k-test.rkt:17:2>)
;; three-k-test.rkt﻿> (define kons (curry (λ (a b c) (cons a (c)))))
;; three-k-test.rkt﻿> (define nill (thunk '()))
;; three-k-test.rkt﻿> (((((((disj F (allo)) kons) identity) nill) kons) identity) nill)
;; #<procedure:curried>
;; three-k-test.rkt﻿> (((disj F (allo)) kons) identity)
;; #<procedure:...three-k-test.rkt:19:6>
;; three-k-test.rkt﻿> ((((disj F (allo)) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:17:2>
;; three-k-test.rkt﻿> 
;; three-k-test.rkt﻿> ((((disj F (allo)) kons) identity) nill)
;; ; kons: undefined;
;; ;  cannot reference an identifier before its definition
;; ;   in module: "/Users/jhemann/311/three-k-test.rkt"
;; three-k-test.rkt﻿> (define kons (curry (λ (a b c) (cons a (c)))))
;; three-k-test.rkt﻿> (define nill (thunk '()))
;; three-k-test.rkt﻿> ((((disj F (allo)) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:18:2>
;; three-k-test.rkt﻿> (((S kons) identity) nill)
;; '(#t)
;; three-k-test.rkt﻿> ((((disj F S) kons) identity) nill)
;; '(#t)
;; three-k-test.rkt﻿> ((((disj S S) kons) identity) nill)
;; '(#t #t)
;; three-k-test.rkt﻿> ((((disj (allo) S) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:18:2>
;; three-k-test.rkt﻿> (((((((disj (allo) S) kons) identity) nill) kons) identity) nill)
;; '(#t #t . #<procedure:...three-k-test.rkt:18:2>)
;; three-k-test.rkt﻿> (((((((conj (allo) S) kons) identity) nill) kons) identity) nill)
;; ; conj: undefined;
;; ;  cannot reference an identifier before its definition
;; ;   in module: "/Users/jhemann/311/three-k-test.rkt"
;; three-k-test.rkt﻿> (((((((bind (allo) S) kons) identity) nill) kons) identity) nill)
;; ; application: not a procedure;
;; ;  expected a procedure that can be applied to arguments
;; ;   given: #t
;; ;   arguments...:
;; ;    #t
;; ; Context:
;; ;  /Users/jhemann/311/three-k-test.rkt:30:6
;; ;  /Users/jhemann/311/three-k-test.rkt:11:17
;; three-k-test.rkt﻿> ((((bind (allo) S) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:6:2>
;; three-k-test.rkt﻿> (((((((bind (allo) S) kons) identity) nill) kons) identity) nill)
;; ; application: not a procedure;
;; ;  expected a procedure that can be applied to arguments
;; ;   given: #t
;; ;   arguments...:
;; ;    #t
;; ; Context:
;; ;  /Users/jhemann/311/three-k-test.rkt:30:6
;; ;  /Users/jhemann/311/three-k-test.rkt:11:17
;; three-k-test.rkt﻿> ((((bind (disj S (allo)) S) kons) identity) nill)
;; ; application: not a procedure;
;; ;  expected a procedure that can be applied to arguments
;; ;   given: #t
;; ;   arguments...:
;; ;    #t
;; ; Context:
;; ;  /Users/jhemann/311/three-k-test.rkt:30:6
;; ;  /Users/jhemann/311/three-k-test.rkt:11:17
;; three-k-test.rkt﻿> ((((bind S S) kons) identity) nill)
;; ; application: not a procedure;
;; ;  expected a procedure that can be applied to arguments
;; ;   given: #t
;; ;   arguments...:
;; ;    #t
;; ; Context:
;; ;  /Users/jhemann/311/three-k-test.rkt:30:6
;; ;  /Users/jhemann/311/three-k-test.rkt:11:17
;; three-k-test.rkt﻿> 
;; three-k-test.rkt﻿> ((((bind S (λ (_) S)) kons) identity) nill)
;; ; kons: undefined;
;; ;  cannot reference an identifier before its definition
;; ;   in module: "/Users/jhemann/311/three-k-test.rkt"
;; three-k-test.rkt﻿> 
;; three-k-test.rkt﻿> ((((disj F (allo)) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:24:2>
;; three-k-test.rkt﻿> (((((((disj F (allo)) kons) identity) nill) kons) identity) nill)
;; '(#t . #<procedure:...three-k-test.rkt:24:2>)
;; three-k-test.rkt﻿> ((((cdr (((((((disj F (allo)) kons) identity) nill) kons) identity) nill)) kons)identity) nill)
;; '(#t . #<procedure:...three-k-test.rkt:24:2>)
;; three-k-test.rkt﻿> ((((bind (unit #t) unit) kons) identity) nill)
;; '(#t)
;; three-k-test.rkt﻿> 
;; three-k-test.rkt﻿> ((((bind S S) kons) identity) nill)
;; ; application: not a procedure;
;; ;  expected a procedure that can be applied to arguments
;; ;   given: #t
;; ;   arguments...:
;; ;    #t
;; ; Context:
;; ;  /Users/jhemann/311/three-k-test.rkt:36:6
;; ;  /Users/jhemann/311/three-k-test.rkt:17:17
;; three-k-test.rkt﻿> ((((bind S (λ (_) S)) kons) identity) nill)
;; '(#t)
;; three-k-test.rkt﻿> ((((bind (allo #t) (λ (_) S)) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:12:2>
;; three-k-test.rkt﻿> (((((((bind (allo #t) (λ (_) S)) kons) identity) nill) kons) identity) nill)
;; '(#t . #<procedure:...three-k-test.rkt:12:2>)
;; three-k-test.rkt﻿> ((((cdr (((((((bind (allo #t) (λ (_) S)) kons) identity) nill) kons) identity) nill)) kons) identity) nill) 
;; '(#t . #<procedure:...three-k-test.rkt:12:2>)
;; three-k-test.rkt﻿> (((((((bind (disj S (disj S S)) (λ (_) S)) kons) identity) nill) kons) identity) nill)
;; ; application: not a procedure;
;; ;  expected a procedure that can be applied to arguments
;; ;   given: '(#t #t #t)
;; ;   arguments...:
;; ;    #<procedure:curried>
;; three-k-test.rkt﻿> ((((bind (disj S (disj S S)) (λ (_) S)) kons) identity) nill)
;; '(#t #t #t)
;; three-k-test.rkt﻿> 
;; three-k-test.rkt﻿> ((((bind (disj S (disj (moreo #t) S)) (λ (_) S)) kons) identity) nill)
;; '(#t . #<procedure:...three-k-test.rkt:10:2>)
;; three-k-test.rkt﻿> ((((bind (disj (moreo #t) S) (λ (_) S)) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:10:2>
;; three-k-test.rkt﻿> (((((((bind (disj (moreo #t) S) (λ (_) S)) kons) identity) nill) kons) identity) nil)
;; ; nil: undefined;
;; ;  cannot reference an identifier before its definition
;; ;   in module: "/Users/jhemann/311/three-k-test.rkt"
;; three-k-test.rkt﻿> (((((((bind (disj (moreo #t) S) (λ (_) S)) kons) identity) nill) kons) identity) nill)
;; '(#t . #<procedure:...three-k-test.rkt:10:2>)
;; three-k-test.rkt﻿> ((((cdr (((((((bind (disj (moreo #t) S) (λ (_) S)) kons) identity) nill) kons) identity) nill)) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:10:2>
;; three-k-test.rkt﻿> (((((((cdr (((((((bind (disj (moreo #t) S) (λ (_) S)) kons) identity) nill) kons) identity) nill)) kons) identity) nill) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:10:2>
;; three-k-test.rkt﻿> ((((bind (allo #t) allo) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:10:2>
;; three-k-test.rkt﻿> (((((((bind (allo #t) allo) kons) identity) nill) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:10:2>
;; three-k-test.rkt﻿> ((((((((((bind (allo #t) allo) kons) identity) nill) kons) identity) nill) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:10:2>
;; three-k-test.rkt﻿> (((((((((((((bind (allo #t) allo) kons) identity) nill) kons) identity) nill) kons) identity) nill) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:10:2>
;; three-k-test.rkt﻿> ((((allo #t) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:22:2>
;; three-k-test.rkt﻿> (((((((allo #t) kons) identity) nill) kons) identity) nill)
;; '(#t . #<procedure:...three-k-test.rkt:22:2>)
;; three-k-test.rkt﻿> 
;; three-k-test.rkt﻿> (((((((allo 5) kons) identity) nill) kons) identity) nill)
;; '(#t . #<procedure:...three-k-test.rkt:23:2>)
;; three-k-test.rkt﻿> (((((((nats 5) kons) identity) nill) kons) identity) nill)
;; '(5 . #<procedure:...three-k-test.rkt:23:2>)
;; three-k-test.rkt﻿> (((kons (cdr (((((((nats 5) kons) identity) nill) kons) identity) nill))) identity) nill)
;; '(#<procedure:...three-k-test.rkt:23:2>)
;; three-k-test.rkt﻿> ((((cdr (((((((nats 5) kons) identity) nill) kons) identity) nill)) kons) identity) nill)
;; '(6 . #<procedure:...three-k-test.rkt:23:2>)
;; three-k-test.rkt﻿> ((((bind (nats 5) nats) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:11:2>
;; three-k-test.rkt﻿> (((((((bind (nats 5) nats) kons) identity) nill) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:11:2>
;; three-k-test.rkt﻿> ((((nats 5) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:11:2>
;; three-k-test.rkt﻿> ((((nats 5) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:23:2>
;; three-k-test.rkt﻿> ((((((((((nats 5) kons) identity) nill) kons) identity) nill) kons) identity) nill)
;; ; application: not a procedure;
;; ;  expected a procedure that can be applied to arguments
;; ;   given: '(5 . #<procedure:...three-k-test.rkt:23:2>)
;; ;   arguments...:
;; ;    #<procedure:curried>
;; three-k-test.rkt﻿> (((((((nats 5) kons) identity) nill) kons) identity) nill)
;; '(5 . #<procedure:...three-k-test.rkt:23:2>)
;; three-k-test.rkt﻿> ((((nats 5) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:23:2>
;; three-k-test.rkt﻿> ((((bind (nats 5) unit) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:11:2>
;; three-k-test.rkt﻿> (((((((bind (nats 5) unit) kons) identity) nill) kons) identity) nill)
;; '(5 . #<procedure:...three-k-test.rkt:11:2>)
;; three-k-test.rkt﻿> (((((((bind (nats 5) nats) kons) identity) nill) kons) identity) nill)
;; #<procedure:...three-k-test.rkt:11:2>
;; three-k-test.rkt﻿> ((((cdr (((((((bind (nats 5) unit) kons) identity) nill) kons) identity) nill)) kons) identity) nill)
;; '(6 . #<procedure:...three-k-test.rkt:11:2>)
;; three-k-test.rkt﻿> 
;; three-k-test.rkt﻿> (looper (bind (nats 5) nats))
;;   C-c C-c  C-c C-c; user break
;; ; Context:
;; ;  /Users/jhemann/311/three-k-test.rkt:74:2 looper
;; three-k-test.rkt﻿> (looper (bind (nats 5) unit))
;; '(5 . #<procedure:...three-k-test.rkt:11:2>)
;; three-k-test.rkt﻿> (((((((bind (nats 5) nats) kons) identity) nill) kons) identity) nill)

;; three-k-test.rkt﻿> (f (nat1 5))
;; '(5 6 7)
;; three-k-test.rkt﻿> 
;; three-k-test.rkt﻿> (looper (nat1 5))
;; '(5 6 7)
;; three-k-test.rkt﻿> (looper (nat2 5))
;; '(5 6)
;; three-k-test.rkt﻿> 
;; (((((((nat3 3) kons) identity) nill) kons) identity) nill)
(looper (bind (unit 5) nat3))



;; (trace-define (bind c f)
;;   (λ (mk)
;;     (λ (lk)
;;       (λ (ek)
;;         (((c (λ (a) 
;;                (λ (ek)
;;                  ((((f a)
;;                     mk) ;; what I do w/the answer I get
;;                    (λ (c) ;; what I do w/a delay
;;                      (lk
;;                       (λ (mk)
;;                         (λ (lk)
;;                           (λ (ek) ;; a computation
;;                             ;; so if we kept it in an fk, it'd require an unbounded amount of work.
;;                             ;; that will run what we do when we flip the value
;;                             (λ (c2) ;; together with this c in a failure continuation
;;                               (((c mk) 
;;                                 lk)
;;                                (ek c2)))))))))
;;                   ek)))) ;; what I do if we fail
;;           (trace-lambda #:name should-only-go-once (c)
;;             (lk (bind c f))))
;;          ek)))))
