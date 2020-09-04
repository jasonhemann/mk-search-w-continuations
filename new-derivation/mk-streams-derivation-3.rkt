#lang racket

#| 

Here we install the recursive delay operator, that is the additional
superpower endowed by this model.

Unlike the others, this *does* require a macro to implement this
operator. I expect all 4 versions, in all 4 modules, will require
macros to implement. 

Notice that this define-relation does /not/ have the state embedded in
it. None of them mention the state right now. Not sure how that worked
or if they needed to, but for just basic stream operations, we
needn't.

|#

(module streams-unit-map-join racket
  (require (combine-in rackunit racket/promise))
  (provide (all-defined-out))

  (define-syntax-rule (define-relation (n . args) g)
    (define (n . args) (delay/name g)))
  
  (define (unit a) (list a))

  (define ((map f) m)
    (cond
      ((null? m) '())
      ((promise? m) (delay/name (join ((map f) (force m))))) 
      ((cons? m) (cons (f (car m)) ((map f) (cdr m))))))

  (define (join mma)
    (cond
      ((null? mma) '())
      ((promise? mma) mma)
      ((cons? mma) (mplus (car mma) (join (cdr mma))))))

  (define (mzero) '())
  
  (define (mplus m1 m2)
    (cond
      ((null? m1) m2)
      ((promise? m1) (delay/name (mplus m2 (force m1))))
      ((cons? m1) (cons (car m1) (mplus (cdr m1) m2)))))

  (define (return a)
    (unit a))

  (define ((bind m) f)
    (join ((map f) m)))
  )

(module streams-bind-return racket
  (require (combine-in rackunit racket/promise))
  (provide (all-defined-out))

  (define-syntax-rule (define-relation (n . args) g)
    (define (n . args) (delay/name g)))
  
  (define (return a) (cons a '()))

  (define (mzero) '())
  
  (define ((bind m) f)
    (cond
      ((null? m) '())
      ((promise? m) (delay/name ((bind (force m)) f)))
      ((cons? m) (mplus (f (car m)) ((bind (cdr m)) f)))))

  (define (mplus m1 m2)
    (cond
      ((null? m1) m2)
      ((promise? m1) (delay/name (mplus m2 (force m1))))
      ((cons? m1) (cons (car m1) (mplus (cdr m1) m2)))))
  
  (define (unit a)
    (return a))

  (define ((map f) m)
    ((bind m) (λ (a) (unit (f a)))))

  (define (join z)
    ((bind z) (λ (a) a)))
  )

(module sk/fk-unit-map-join racket
  (require (combine-in rackunit racket/trace))
  (provide (all-defined-out))

  (define-syntax-rule (define-relation (n . args) g)
    (define (n . args)
      (λ (dk)
        (λ (sk)
          (λ (fk)
            (dk g))))))

  (define (unit a)
    (λ (dk)
      (λ (sk)
        (λ (fk)
          ((sk a) fk)))))

  (define (mzero)
    (λ (dk)
      (λ (sk)
        (λ (fk)
          (fk)))))

  (define (mplus m1 m2)
    (λ (dk)
      (λ (sk)
        (λ (fk)
          (((m1
             (λ (c1^)
               (dk (mplus m2 c1^))))
            sk)
           (λ ()
             (((m2 dk) sk) fk)))))))
  
  (define ((map f) m)
    (λ (dk)
      (λ (sk)
        (λ (fk)
          (((m
             (λ (m^)
               (dk ((map f) m^)))) 
            (λ (b)
              (λ (fk)
                ((sk (f b)) fk))))
           fk)))))

  (define (join mma)
    (λ (dk)
      (λ (sk)
        (λ (fk)
          (((mma
             (λ (mm^)
               (dk (join mm^))))
            (λ (mb)
              (λ (fk)
                ((mb sk) fk))))
           fk)))))

  (define (return a)
    (unit a))

  (define ((bind m) f)
    (join ((map f) m)))
  )

(module sk/fk-bind-return racket
  (require rackunit)
  (provide (all-defined-out))

  (define-syntax-rule (define-relation (n . args) g)
    (define (n . args)
      (λ (dk)
        (λ (sk)
          (λ (fk)
            (dk g))))))
  
  (define (return a)
    (λ (dk)
      (λ (sk)
        (λ (fk)
          ((sk a) fk)))))

  (define ((bind m) f)
    (λ (dk)
      (λ (sk)
        (λ (fk)
          (((m
             (λ (m^)
               (dk ((bind m^) f))))
            (λ (b)
              (λ (fk)
                (((f b)
                  sk)
                 fk))))
           fk)))))

  (define (mzero)
    (λ (dk)
      (λ (sk)
        (λ (fk)
          (fk)))))

  (define (mplus m1 m2)
    (λ (dk)
      (λ (sk)
        (λ (fk)
          (((m1
             (λ (c1^)
               (dk (mplus m2 c1^))))
            sk)
           (λ ()
             (((m2 dk) sk) fk)))))))

  (define (unit a)
    (return a))

  (define ((map f) m)
    ((bind m) (λ (a) (unit (f a)))))

  (define (join z)
    ((bind z) (λ (a) a)))
  )
