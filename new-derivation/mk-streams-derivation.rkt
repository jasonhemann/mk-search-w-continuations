#lang racket

#| 

Here, however, in this version, both failure continuations and delay
continuations return computations to be invoked. This is because
failure actually does adjust the context in which the computation is
evaluated.

I want to confirm, however, that this actually *does* still maintain
the property of continuously producing answers up til the first delay,
and *that* these are invoked until then. 

It seems like we are `inc`ing with every delay, but just
resuming. Which is to say, we have a delay inside every computation
that comes back. 

Is that like saying you'd return a single non-empty *list* of answers,
together with the delay as a failure continuation? Is this what I
wanted?

(define-relation (l n)
  (mplus (unit n) (mplus (unit n) (l n))))

(length (run 3 (mplus (l 5) (l 6))))

|# 

(module streams-unit-map-join racket
  (require (combine-in rackunit racket/promise racket/trace))
  (provide (all-defined-out))

  ;; Ugly external interface for cover
  (define (run . args)
    (cond
      ((null? (cdr args)) (loop* (car args)))
      (else ((loop (car args)) (cadr args)))))

  (define (loop* c)
    (cond
      ((null? c) '())
      ((promise? c) (loop* (force c)))
      ((cons? c) (cons (car c) (loop* (cdr c))))))

  (define ((loop n) c)
    (cond
      ((null? c) '())
      ((promise? c)
       (if (zero? n)
           '()
           ((loop (sub1 n)) (force c))))
      ((cons? c)
       (cons (car c) ((loop n) (cdr c))))))
  
  ;; (define ((walk-ans n) c)
  ;;   (cond
  ;;     ((null? c) '())
  ;;     ((cons? c) (cons (car c) ((walk-ans n) (cdr c))))
  ;;     (else ((loop n) c))))

  ;; (define ((loop n) c)
  ;;   (cond
  ;;     ((promise? c)
  ;;      (if (zero? n) '()
  ;;          ((loop (sub1 n)) (force c))))
  ;;     (else ((walk-ans n) c))))
  
  (define-syntax-rule (define-relation (n . args) g)
    (define (n . args) (delay/name g)))

  (define-syntax-rule (freeze e) (delay/name e))
  
  (define (unit a) (list a))

  (define ((map f) m)
    (cond
      ((null? m) '())
      ((promise? m) (delay/name ((map f) (force m))))
      ((cons? m) (cons (f (car m)) ((map f) (cdr m))))))

  (define (join mma)
    (cond
      ((null? mma) '()) 
      ((promise? mma) (delay/name (join (force mma))))
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
  (require racket/trace)
  (provide (all-defined-out))

  ;; Ugly external interface for cover
  (define (run . args)
    (cond
      ((null? (cdr args)) (loop* (car args)))
      (else ((loop (car args)) (cadr args)))))

  (define (loop* c)
    (cond
      ((null? c) '())
      ((promise? c) (loop* (force c)))
      ((cons? c) (cons (car c) (loop* (cdr c))))))

  (define ((loop n) c)
    (cond
      ((null? c) '())
      ((promise? c)
       (if (zero? n)
           '()
           ((loop (sub1 n)) (force c))))
      ((cons? c)
       (cons (car c) ((loop n) (cdr c))))))

  (define-syntax-rule (define-relation (n . args) g)
    (define (n . args) (delay/name g)))

  (define-syntax-rule (freeze e) (delay/name e))
  
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
  (require racket/trace)
  (require rackunit)
  (provide (all-defined-out))

  (define (run . args)
    (cond
      ((null? (cdr args)) (loop* (car args)))
      (else ((loop (car args)) (cadr args)))))
  
  (define (loop n)
    (λ (c)
      (((c
         (λ (c^)
           (if (zero? n)
               '()
               ((loop (sub1 n)) c^))))
        (λ (a)
          (λ (c)
            (cons a ((loop n) c)))))
       (λ ()
         '()))))

  (define loop*
    (λ (c)
      (((c
         (λ (c^)
           (loop* c^)))
        (λ (a)
          (λ (c)
            (cons a (loop* c)))))
       (λ ()
         '()))))

  (define-syntax-rule (define-relation (n . args) g)
    (define (n . args)
      (λ (dk)
        (λ (sk)
          (λ (fk)
            (dk g))))))

  (define-syntax-rule (freeze e) 
      (λ (dk)
        (λ (sk)
          (λ (fk)
            (dk e)))))
  
  (define (unit a)
    (λ (dk)
      (λ (sk)
        (λ (fk)
          ((sk a) (mzero))))))

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
            (λ (a)
              (λ (c)
                ((sk a) (mplus c m2)))))
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
		(λ (c)
		   ((sk (f b)) ((map f) c)))))
            fk)))))

  (define (join mm)
    (λ (dk)
      (λ (sk)
        (λ (fk)
          (((mm
             (λ (mm^)
               (dk (join mm^))))
            (λ (mb)
              (λ (c)
                ((((mplus mb (join c)) 
                   dk)
                  sk)
                 fk))))
           fk)))))

  (define (return a)
    (unit a))

  (define ((bind m) f)
    (join ((map f) m)))
  )

(module sk/fk-bind-return racket
  (require rackunit)
  (require racket/trace)
  (provide (all-defined-out))

  (define (run . args)
    (cond
      ((null? (cdr args)) (loop* (car args)))
      (else ((loop (car args)) (cadr args)))))

  (define (loop n)
    (λ (c)
      (((c
         (λ (c^)
           (if (zero? n)
               '()
               ((loop (sub1 n)) c^))))
        (λ (a)
          (λ (c)
            (cons a ((loop n) c)))))
       (λ ()
         '()))))

  (define loop*
    (λ (c)
      (((c
         (λ (c^)
           (loop* c^)))
        (λ (a)
          (λ (c)
            (cons a (loop* c)))))
       (λ ()
         '()))))

  (define-syntax-rule (define-relation (n . args) g)
    (define (n . args)
      (λ (dk)
        (λ (sk)
          (λ (fk)
            (dk g))))))

  ;; not an essential operator
  (define-syntax-rule (freeze e) 
    (λ (dk)
      (λ (sk)
        (λ (fk)
          (dk e)))))

  (define (return a)
    (λ (dk)
      (λ (sk)
        (λ (fk)
          ((sk a) (mzero))))))
  
  (define ((bind m) f)
    (λ (dk)
      (λ (sk)
        (λ (fk)
          (((m
             (λ (m^)
               (dk ((bind m^) f))))
            (λ (b)
              (λ (c)
                ((((mplus (f b) ((bind c) f))
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
            (λ (a)
              (λ (c)
                ((sk a) (mplus c m2)))))
           (λ ()
             (((m2 dk) sk) fk)))))))

  ;; Cf b/c this has a recursion without the delay
  ;; faster-mk sytle definition of interleaving mplus (w/o delay,
  ;; interleave on every)
  
  ;; (define (mplus m1 m2)
  ;;   (λ (sk)
  ;;     (λ (fk)
  ;;       ((m1 (λ (a)
  ;;              (λ (fk)
  ;;                ((sk a)
  ;;                 (λ ()
  ;;                   (mplus m2 (fk)))))))
  ;;        (λ ()
  ;;          ((m2 sk)
  ;;           fk))))))
  
  (define (unit a)
    (return a))

  (define ((map f) m)
    ((bind m) (λ (a) (unit (f a)))))

  (define (join z)
    ((bind z) (λ (a) a)))
  )



