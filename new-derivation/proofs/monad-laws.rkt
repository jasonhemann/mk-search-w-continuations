#lang racket

#| 

In this file I was going to try to use equational reasoning to show
that our putative monad does in fact satisfy the monadic laws. Got
stuck. I wonder if there's any insight from looking at the hand-traces
of the streams-based implementations' demonstrations, and working with
that.

After doing this, we would have to also argue that our monad satisfies
whatever mplus/mzero laws we dictate that it should (those are
somewhat under discussion).

"Backtracking, interleaving" w/ Hughes "The design of a
pretty-printing library" both describe essentially the following:

Hughes, §6

Fail: Ma 
orelse: Ma -> Ma -> Ma

fail `orelse` x = x
x `orelse` fail = x
(x `orelse` y) `orelse` z = x `orelse` (y `orelse` z)

fail `bind` f = fail 
(x `orelse` y) `bind` f = (x `bind` f) `orelse` (y `bind` f)

And value changes from `value: Ma -> a` to `value: Ma -> Maybe a`
because fail doesn't have a value. "Backtracking interleaving .."
gives a different return, with a finite subset of a list of answers. 

But the discussion comes up, nicely documented, on the Haskell wiki.

https://web.archive.org/web/20070701183929/http://haskell.org:80/hawiki/MonadPlus
https://web.archive.org/web/20070610000147/http://www.haskell.org/hawiki/NonDeterminism
https://web.archive.org/web/20051127193356/http://haskell.org/hawiki/DataStructureFreeTransformation
https://web.archive.org/web/20061011013348/http://www.haskell.org/hawiki/ListTDoneRight

|# 

#| 

Fixpoint operators, and that the definition of bind uses mplus in it, will I believe make this a bitch. Will
this require a nested proof by induction?

Left identity: return a >>= h ≡ h a

Right identity: m >>= return ≡ m

Associativity: (m >>= g) >>= h ≡ m >>= (\x -> g x >>= h)

|# 


(define (fix f)
  (letrec ([g (λ (x) ((f g) x))])
    g))

(define (fix2 f) 
  (letrec ([g (λ (x y) ((f g) x y))])
    g))

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

#| 

Right identity: m >>= return ≡ m

(bind m return) 
= expand definition of bind.
((fix2
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
         fk)))))
 m 
 return)
= expand fix definition 1x 
((λ (m f)
  (λ (dk sk fk)
    (m
     (λ (m^)
       (dk (bind m^ f)))
     (λ (b vc c)
       ((mplus (f b vc) (bind c f))
        dk
        sk
        fk))
     fk)))
 m 
 return)
= subst in redex
(λ (dk sk fk)
  (m
   (λ (m^)
     (dk (bind m^ return)))
   (λ (b vc c)
     ((mplus (return b vc) (bind c return))
      dk
      sk
      fk))
   fk))
= Proof by cases. 
Computation `m` invokes one of three continuations.

1. Given any three dk, sk, fk continuations, m will invoke the dk continunation 

(λ (dk sk fk)
  (m
   (λ (m^)
     (dk (bind m^ return)))
   (λ (b vc c)
     ((mplus (return b vc) (bind c return))
      dk
      sk
      fk))
   fk))
= since m will invoke the dk continuation, we reduce the redex ("do the application")
(λ (dk sk fk)
  (dk (bind m return)))
= This uses an inductive hypothesis, I belive. 
(λ (dk sk fk)
  (dk m))
= in the context of any three such continuations, this gives 
m

2. Given any three dk, sk, fk continuations, m will invoke the sk continunation 

(λ (dk sk fk)
  (m
   (λ (m^)
     (dk (bind m^ return)))
   (λ (b vc c)
     ((mplus (return b vc) (bind c return))
      dk
      sk
      fk))
   fk))
= m must be a computation with at least one answer, m =_df b vc c
(λ (dk sk fk)
  ((mplus (return b vc) (bind c return))
   dk
   sk
   fk))
= by eta reduction
(mplus (return b vc) (bind c return))
= By I think the inductive hypothesis
(mplus (return b vc) c)
= by the definition of mplus 
((fix2 
   (λ (mplus)
     (λ (m1 m2)
       (λ (dk sk fk)
         (m1
          (λ (c1^)
            (dk (mplus m2 c1^)))
          (λ (a vc c)
            (sk a vc (mplus c m2)))
          (λ ()
            (m2 dk sk fk)))))))
 (return b vc)
 c)
= Unfolding the fix definition 1x
((λ (m1 m2)
  (λ (dk sk fk)
    (m1
     (λ (c1^)
       (dk (mplus m2 c1^)))
     (λ (a vc c)
       (sk a vc (mplus c m2)))
     (λ ()
       (m2 dk sk fk)))))
 (return b vc)
 c)
=  reducing the body of mplus 
  (λ (dk sk fk)
    ((return b vc)
     (λ (c1^)
       (dk (mplus c c1^)))
     (λ (a vc c2)
       (sk a vc (mplus c2 c)))
     (λ ()
       (c dk sk fk))))
= expanding the definition of return
  (λ (dk sk fk)
    ((λ (dk sk fk)
       (sk b vc (mzero)))
     (λ (c1^)
       (dk (mplus c c1^)))
     (λ (a vc c2)
       (sk a vc (mplus c2 c)))
     (λ ()
       (c dk sk fk))))
= reducing return
  (λ (dk sk fk)
    ((λ (a vc c2)
       (sk a vc (mplus c2 c))
       b
       vc 
       (mzero))))
= reducing the success continuation
(λ (dk sk fk)
 (sk b vc (mplus (mzero) c)))
= assuming our mplus mzero c = c law
(λ (dk sk fk)
 (sk b vc c))
= by our definition of m =_df b vc c
m

3. Given any three dk, sk, fk continuations, m will invoke the fk continunation 

(λ (dk sk fk)
  (m
   (λ (m^)
     (dk (bind m^ return)))
   (λ (b vc c)
     ((mplus (return b vc) (bind c return))
      dk
      sk
      fk))
   fk))
= after reducing m by application
(λ (dk sk fk) 
  fk)
= pretty trivially, then
m

|#

#| 
Left identity: return a vc >>= h ≡ h a vc

(bind (return a vc) h)
= by the definition of bind
((fix2
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
         fk)))))
 (return a vc)
 h)
= unfolding the fix-point definition once (assuming bind has its global defn)
((λ (m f)
   (λ (dk sk fk)
     (m
      (λ (m^)
        (dk (bind m^ f)))
      (λ (b vc c)
        ((mplus (f b vc) (bind c f))
         dk
         sk
         fk))
      fk)))
 (return a vc)
 h)
= reducing the redex
(λ (dk sk fk)
  ((return a vc)
   (λ (m^)
     (dk (bind m^ h)))
   (λ (b vc c)
     ((mplus (h b vc) (bind c h))
      dk
      sk
      fk))
   fk))
= by the definition of return
(λ (dk sk fk)
  ((λ (dk sk fk)
    (sk a vc (mzero)))
   (λ (m^)
     (dk (bind m^ h)))
   (λ (b vc c)
     ((mplus (h b vc) (bind c h))
      dk
      sk
      fk))
   fk))
= reduce the application of return
(λ (dk sk fk)
  ((λ (b vc c)
     ((mplus (h b vc) (bind c h))
      dk
      sk
      fk))
   a 
   vc 
   (mzero)))
= reduce the application of sk
(λ (dk sk fk)
  ((mplus (h a vc) (bind (mzero) h))
   dk
   sk
   fk))
= eta reduction
(mplus (h a vc) (bind (mzero) h))
= assuming our (mzero) `bind` h = h law
(mplus (h a vc) (mzero))
= assuming our x `orelse` fail = x law
(h a vc)

|#

#| 

(define (return a vc)
  (λ (dk sk fk)
    (sk a vc (mzero))))


;; (>>= (>>= m g) h) ≡ (>>= m (\x -> (>>= (g x) h)))
(bind m (lambda (x y) (bind (g x y) h)))
= by the definition of bind
((fix2
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
         fk)))))
 m
 (lambda (x)
   (bind (g x) h)))
=  unfolding bind 1x
((λ (m f)
   (λ (dk sk fk)
     (m
      (λ (m^)
        (dk (bind m^ f)))
      (λ (b vc c)
        ((mplus (f b vc) (bind c f))
         dk
         sk
         fk))
      fk)))
 m
 (lambda (x)
   (bind (g x) h)))
= reducing unfolded bind
(λ (dk sk fk)
  (m
   (λ (m^)
     (dk (bind m^ (lambda (x) (bind (g x) h)))))
   (λ (b vc c)
     ((mplus ((lambda (x) (bind (g x) h)) b vc)
             (bind c (lambda (x) (bind (g x) h))))
      dk
      sk
      fk))
   fk))
= because I realized that this was actually supposed to be the 2-arg bind I invented
(λ (dk sk fk)
  (m
   (λ (m^)
     (dk (bind m^ (lambda (x y) (bind (g x y) h)))))
   (λ (b vc c)
     ((mplus (bind (g b vc) h)
             (bind c (lambda (x y) (bind (g x y) h))))
      dk
      sk
      fk))
   fk))
=Proof by cases. Because there are three ways to construct an m

1. m is a computation that executes the dk

(λ (dk sk fk)
  (m
   (λ (m^)
     (dk (bind m^ (lambda (x y) (bind (g x y) h)))))
   (λ (b vc c)
     ((mplus (bind (g b vc) h)
             (bind c (lambda (x y) (bind (g x y) h))))
      dk
      sk
      fk))
   fk))
=
(λ (dk sk fk)
  (dk (bind m (lambda (x y) (bind (g x y) h)))))

2. m is a computation that consumes the sk 

(λ (dk sk fk)
  (m
   (λ (m^)
     (dk (bind m^ (lambda (x y) (bind (g x y) h)))))
   (λ (b vc c)
     ((mplus (bind (g b vc) h)
             (bind c (lambda (x y) (bind (g x y) h))))
      dk
      sk
      fk))
   fk))

= assuming m =_df b vc c
(λ (dk sk fk)
  ((mplus (bind (g b vc) h)
               (bind c (lambda (x y) (bind (g x y) h))))
        dk
        sk
        fk))
= eta reduction to 
(mplus (bind (g b vc) h)
       (bind c (lambda (x y) (bind (g x y) h))))

3. m is a computation that consumes the fk 
(λ (dk sk fk)
  fk)


|# 
