#lang racket

#| 

_Entirely_ sure I'm not happy with the run

Not _entirely_ sure I'm happy with the walk-ans-es

|#

(module streams-unit-map-join racket
  (require (combine-in rackunit racket/promise))
  (provide (all-defined-out))

  ;; Ugly external interface for cover
  (define (run . args)
    (cond
      ((null? (cdr args)) (loop* (car args)))
      (else ((loop (car args)) (cadr args)))))
  
  (define (walk-ans* c)
    (cond
      ((null? c) '())
      ((cons? c) (cons (car c) (walk-ans* (cdr c))))
      (else (loop* c))))

  (define (loop* c)
    (cond
      ((promise? c) (loop* (force c)))
      (else (walk-ans* c))))

  (define ((walk-ans n) c)
    (cond
      ((null? c) '())
      ((cons? c) (cons (car c) ((walk-ans n) (cdr c))))
      (else ((loop n) c))))

  (define ((loop n) c)
    (cond
      ((promise? c)
       (if (zero? n) '()
           ((loop (sub1 n)) (force c))))
      (else ((walk-ans n) c))))
  
  (define-syntax-rule (define-relation (n . args) g)
    (define (n . args) (delay/name g)))
  
  (define (unit a) (list a))

  (define ((map m) f)
    (cond
      ((null? m) '())
      ((promise? m) (delay/name ((map m) f))) 
      ((cons? m) (cons (f m) ((map (cdr m)) f)))))

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

  ;; Ugly external interface for cover
  (define (run . args)
    (cond
      ((null? (cdr args)) (loop* (car args)))
      (else ((loop (car args)) (cadr args)))))
  
  (define (walk-ans* c)
    (cond
      ((null? c) '())
      ((cons? c) (cons (car c) (walk-ans* (cdr c))))
      (else (loop* c))))

  (define (loop* c)
    (cond
      ((promise? c) (loop* (force c)))
      (else (walk-ans* c))))

  (define ((walk-ans n) c)
    (cond
      ((null? c) '())
      ((cons? c) (cons (car c) ((walk-ans n) (cdr c))))
      (else ((loop n) c))))

  (define ((loop n) c)
    (cond
      ((promise? c)
       (if (zero? n) '()
           ((loop (sub1 n)) (force c))))
      (else ((walk-ans n) c))))

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
  (require rackunit)
  (provide (all-defined-out))

  ;; Ugly external interface for cover
  (define (run . args)
    (cond
      ((null? (cdr args)) (loop* (car args)))
      (else ((loop (car args)) (cadr args)))))
  
  (define kons (λ (a) (λ (fk) (cons a (fk)))))
  (define nill (λ () '()))

  (define (loop n)
    (λ (c)
      (if (zero? n)
          '()
          (((c (loop (sub1 n))) kons) nill))))

  (define loop*
    (λ (c)
      (((c loop*) kons) nill)))

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

  ;; Ugly external interface for cover
  (define (run . args)
    (cond
      ((null? (cdr args)) (loop* (car args)))
      (else ((loop (car args)) (cadr args)))))
  
  (define kons (λ (a) (λ (fk) (cons a (fk)))))
  (define nill (λ () '()))

  (define (loop n)
    (λ (c)
      (if (zero? n)
          '()
          (((c (loop (sub1 n))) kons) nill))))

  (define loop*
    (λ (c)
      (((c loop*) kons) nill)))

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
                ((((f b)
                   dk)
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
