#lang racket

#|

We are beginning with two known/presumed good bits of meaningful theoretical code

The first are the advanced stream-based implementations of traditional microKanren

The second are the known good sk/fk continuation implementations.
(Which have the MonadPlus property via Mitch's paper)

We also know the translation between the two different monad definitions & laws
http://www.ccs.neu.edu/home/dherman/browse/projects/derivations/monad-equiv/monad-equiv.pdf



|#

(module streams-unit-map-join racket
  (require (combine-in rackunit racket/promise))
  
  (define (unit a) (list a))

  ;; These definitions came largely from the Danvy paper
  ;; Wasn't immediately clear how to write the promise lines for
  ;; map/join, but got here because map must return a single value
  (define ((map f) m)
    (cond
      ((null? m) '())
      ((promise? m) (delay/name ((map f) m))) 
      ((cons? m) (cons (f m) ((map f) (cdr m))))))


  (define (join mma)
    (cond
      ((null? mma) '())
      ((promise? mma) mma)
      ((cons? mma) (mplus (car mma) (join (cdr mma))))))

  (define (mzero) '())
  
  ;; mplus: Ma -> Ma -> Ma 
  (define (mplus m1 m2)
    (cond
      ((null? m1) m2)
      ((promise? m1) (delay/name (mplus m2 (force m1))))
      ((cons? m1) (cons (car m1) (mplus (cdr m1) m2)))))

  ;; The derived behaviors
  (define (return a)
    (unit a))

  (define ((bind m) f)
    (join ((map f) m)))
  )

(module streams-bind-return racket
  (require (combine-in rackunit racket/promise))
  
  ;; return: a -> Ma
  (define (return a) (cons a '()))

  (define (mzero) '())
  
  ;; bind : Ma -> (a -> Mb) -> Mb
  (define ((bind m) f)
    (cond
      ((null? m) '())
      ((promise? m) (delay/name (bind (force m) f)))
      ((cons? m) (mplus (f (car m)) ((bind (cdr m)) f)))))

  ;; mplus: Ma -> Ma -> Ma 
  (define (mplus m1 m2)
    (cond
      ((null? m1) m2)
      ((promise? m1) (delay/name (mplus m2 (force m1))))
      ((cons? m1) (cons (car m1) (mplus (cdr m1) m2)))))
  
  ;; The derived behaviors
  (define (unit a)
    (return a))

  (define ((map f) m)
    ((bind m) (λ (a) (unit (f a)))))

  (define (join z)
    ((bind z) (λ (a) a)))
  )

(module sk/fk-unit-map-join racket
  (require rackunit)
  
  (define (unit a)
    (λ (sk)
      (λ (fk)
        ((sk a) fk))))

  ;; These two definitions came from Mitch's paper
  (define (mzero)
    (λ (sk)
      (λ (fk)
        (fk))))

  (define (mplus m1 m2)
    (λ (sk)
      (λ (fk)
        ((m1 sk)
         (λ ()
           ((m2 sk)
            fk))))))
  
  (define ((map f) m)
    (λ (sk)
      (λ (fk)
        ((m
          (λ (b)
            (λ (fk)
              ((sk (f b)) fk))))
         fk))))

  (define (join mma)
    (λ (sk)
      (λ (fk)
        ((mma
          (λ (mb)
            (λ (fk)
              ((mb sk) fk))))
         fk))))

  ;; The derived behaviors
  (define (return a)
    (unit a))

  (define ((bind m) f)
    (join ((map f) m)))
  )

  #| 
   BTW: For bind in the next module

   (m
    (λ (b)
      (λ (fk)
        (((f b)
          sk)
         fk))))

  Because m is a computation in sk/fk, so it first needs a success continuation.
  Which we are giving it here.
  This is the appropriate sk, because it first takes a pure we can provide f
  A sk then eats a fk, which this does. (f b) is a computation
  We execute the computation (f a) within the context of the original, smaller sk 
  (So we've grown the sk by function extension) 
  And from within the context of whatever new fk we take in. 
  (See also in Mitch "Relating .." Section 4.2)

  |# 

(module sk/fk-bind-return racket
  (require rackunit)
  
  ;; return: a -> Ma
  (define (return a)
    (λ (sk)
      (λ (fk)
        ((sk a) fk))))

  ;; bind : Ma -> (a -> Mb) -> Mb
  (define ((bind m) f)
    (λ (sk)
      (λ (fk)
        ((m
          (λ (b)
            (λ (fk)
              (((f b)
                sk)
               fk))))
         fk))))

  ;; These two definitions came from Mitch's paper
  (define (mzero)
    (λ (sk)
      (λ (fk)
        (fk))))

  (define (mplus m1 m2)
    (λ (sk)
      (λ (fk)
        ((m1 sk)
         (λ ()
           ((m2 sk)
            fk))))))

  ;; The derived behaviors
  (define (unit a)
    (return a))

  (define ((map f) m)
    ((bind m) (λ (a) (unit (f a)))))

  (define (join z)
    ((bind z) (λ (a) a)))
  )
