#lang racket
;; To do. Take still a mess. 
;; We want to change our take to do a <spacebar> <return> style mechanism

;; inline-fn call
;; pass init values of dk sk fk and c as arguments to looper
;; change looper to take

(define (ee g s)
  (match g
    [`(ore ,g1 ,g2)  (disj (ee g1 s) (ee g2 s))]
    [`(ande ,g1 ,g2) (bind (ee g1 s) (λ (s) (ee g2 s)))]
    [`(succeed)      (unit s)]
    [`(fail)         (fail)]
    [`(threeo)       (mdelay (ee '(twoo) s))]
    [`(twoo)         (mdelay (ee '(oneo) s))]
    [`(oneo)         (mdelay (ee '(succeed) s))]
    [`(alwayso)      (mdelay (ee '(ore (succeed) (alwayso)) s))]))

(define (mdelay c)
  (λ (dk sk fk)
    (λ (f) ;; it's not wrong to start here for finite computations where we ignore the 'f'. This is just typesafety. 
      (dk c))))

(define (unit s)
  (λ (dk sk fk)
    (λ (f) ;; it's not wrong to start here for finite computations where we ignore the 'f'.
      (sk s fk))))

(define (fail)
  (λ (dk sk fk)
    (λ (f)
      (fk))))

(define (disj c c2)
  (λ (dk sk fk)
    (λ (f)
      (c
       (λ (c) (dk (disj c2 c)))
       sk
       (λ () (c2 dk sk fk))))))

(define (bind c g)
  (λ (dk sk fk)
    (c 
     (λ (c) (dk (bind c g)))
     (λ (s fk) ((g s) dk sk fk))
     fk)))

;; Here's where I think things get too simple. 
(define kons (λ (s fk) (cons s (fk))))
(define nill (λ () '()))
(define identity (λ (c) c))

;; So we have to pass what looks like fuel, one at a time.

;; What does fuel look like? It looks like a "keep-going"
;; continuation.

;; So what do we do when we get an answer out of the top? We prepare to cons it. 

;; So you write a program where you pass it a go-on metacontinuation, until you pass it a stop meta-continuation.

;; The trick is that you have to basically CPS and then some the church numerals of take.

;; So we return something accepting either a success or failure continuation?
;; What's it like when we come back with a delay? It should return something acccepting either a more or a done continuation.

;; What should that be? Should that be something that takes a fuel amount to do its work.

;; computation starts from inside out, rather than outside in. 

;; This is re-using the initial continuations, which for us is a no-no.

;; This is a no. answers model, not an amt. fuel model. 

;; (define (take n c init-dk sk fk)
;;   (match n
;;     [`Z (c init-dk sk fk)]
;;     [`(S ,n^) (c (λ (c) (take n^ c init-dk sk fk)) sk fk)])) ;; Could instead nest up computations here

;; (take '(S Z) (ee '(ore (ande (succeed) (succeed)) (fail)) '()) identity kons nill)
;; (take '(S (S (S Z))) (ee '(threeo) '()) identity kons nill)
;; (take '(S Z) (ee '(alwayso) '()) identity kons nill)
;; (take '(S (S (S (S (S (S (S (S (S (S Z)))))))))) (ee '(ore (alwayso) (ore (alwayso) (succeed))) '()) identity kons nill)
;; (take '(S (S (S (S (S (S (S (S (S (S Z)))))))))) (ee '(ande (ore (alwayso) (succeed)) (succeed)) '()) identity kons nill)
;; (take '(S (S (S (S (S (S (S (S (S (S Z)))))))))) (ee '(ande (ore (alwayso) (succeed)) (fail)) '()) identity kons nill)

(define kons
  (λ (s)
    (λ (fk)
      (fk 
       (λ (d)
         (k (cons s d)))))))

;; What should the delay do?
;; Success K seems like the easiest to handle?
;; Z says finish, end of work, get compiled answers.
;; S/blah says do more work. No inf. loops b/c guaranteed to terminate.

;; So what has to happen is that we either return a procedure, which
;; says we can accept more work, or we return a list, which says we've
;; done all the work we can. This list plays the part of an "abort
;; k". It may have some relationship to one of the early Prolog cut
;; operators, too.

;; The computation must be the type of thing that accepts such a
;; continuation, and that continuation determines whether to just end
;; with the quantity of answers we've developed at present, or to
;; continue working for more.

;; This plays the part of our unary int, but passed in as we
;; go.

;; Perhaps it's the same as a cap on the int, or writing our int
;; inside out?

;; Assume c is '(ee (succeed) s)

;; That is presently the value of (unit s), i.e. 
;; (λ (dk sk fk)
;;   (sk s fk))

;; What else might it be, if what we want to do here is to return the
;; ability to do more work or to do no more work?

;; When we start we pass in some initial success and failure continuations.
;; From then on out, what we pass in are more/abort flags. I think these are not the same. 

(λ (unit s)
  (λ (f) ;; This flag lets us decide what to do here

    ;; either we run the success continuation with the result of t
    ;; presume we have an sk, and an fk
    ;; presume sk still takes a, fk
    ;; presume fk is still empty
    ;; our abort is installing a new fk? maybe? 
    ))

;; It will not surprise me if these end up being 1 / 0 or #t / #f

;; (λ (s) (λ (f) s)) (λ (s) (λ (f) f))

;; (λ (s) (λ (z) (s z))) (λ (s) (λ (z) z))

;; It will not surprise me if choosing between these operators leads to interesting different design choices.

;; a wrong idea: ((f (sk s)) (fk))
;; (f (fk)) ;; this will let us either do what's in the fk or whatever we instead determine is the right thing to do.
;; So we can install a new fk when we need; but what should that new fk need to look like? 

;; modified versions
;; This should return something accepting either an answer, or an (abort-k/more-k). 
(define (take c init-dk sk fk)
  (c init-dk sk fk)) ;; Could instead nest up computations here
;; suppose we return a c. Then what do we do with it?
;; It tells us to ether do 1 level of work, or do the failure. 

(take (ee '(oneo) '()) identity kons nill) ;; maybe identity is the right thing to submit here, to start with?
;; but our implementations of kons and nill seem like they're wrong.

;; We need to do, when we get back a computation, is to supply an argument with what to do next, be it prematurely terminate or continue a step.
;; That might want for a church numeral after all, perhaps inside out, to begin with at the top, outer level. 

;; But we should be able to get this behavior working a bit at a time, and then church numeral onto it for giving it a set amount of work. 

;;computation     ;some-sk;some-fk
((ee '(succeed) s) init-sk init-fk)  

;; how do we avoid passing back another success continuation?
;; Well, the delay should know what to do with the present ones.
;; What should it do when we get an answer?
;; How do we prepare for the next one to come in?

;; maybe something like this? 

(define (unit s)
  (λ (dk sk fk)
    (λ (f) ;; it's not wrong to start here for finite computations where we ignore the 'f'.
      (sk s (f fk)))))

;; composable continuation versions?
;; Perhaps, but it'd be nice if we didn't have to. 
(define first-init-k
  (λ (s)
    (λ (res)
      (cons s (res)))))

(define (init-sk s fk)
  (λ (f)
     (let ((res (f fk)))
       (cons s res))))

;; Our continue k might be either ... well, not sure, actually. 
;; I at first thought it might be either 0-more-fuel, or 1-more-fuel.
;; And I thought that was sufficient enough to answer. 
;; But now, I don't know.
;; We want the argument to be either abortive or not. We believe that. 
;; Can we treat success as a time to expend fuel? 
;; Should we treat success as a time to expend fuel? 
;; If success is not abortive, then we either succeed with as many answers as we get and then ask what to do next.
;; Remember this is what happens in our micro system; we may get lots of answers out of "fuel unit".

;; get another answer and cons it onto the rest if you get it?
;; or it might be "get outta here! with what you've got."

;; We need to have the base continuation "floating around" somehow, so
;; that at the very end it'll get sent into this program and 

;; The currently-implemented model of the world is worrying because
;; what if we have more fuel (or answer requests, dunno which) than
;; actual answers we can get, and if we fail finitely? What happens?

;; What is the current computation model?

;; What should we desire to happen?

;; So, to refresh, here's what we're hoping for.

;; We start up a computation, and it needs to take an sk and an fk.
;; Possibly--probably--a dk.

;; And if we do this, then what?

;; Then should it return either an answer, or a computation awaiting more fuel?

;; Or maybe, if we have an answer, we should just "waste" the remaining fuel, until we get down to 0? 

;; The currently implemented model of the world isn't that bad.
;; It could be better, and get us down to 1 k. But, it could be better.

;; Need to (re-)read Backtracking, Interleaving ....


