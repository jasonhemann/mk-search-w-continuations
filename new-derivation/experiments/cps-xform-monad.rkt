#lang racket

(module CPS-helpers racket
  (provide (all-defined-out)) ;;  λ_ λ‾ appl‾
  (define-syntax λ_
    (syntax-rules ()
      [(_ a b) (λ a b)]))

  ;; perhaps the unicode overline character doesn't highlight correctly as a part of the name? Or perhaps it's broken.
  (define-syntax λ‾
    (syntax-rules ()
      [(_ a b) (λ a b)]))

  (define-syntax appl‾
    (syntax-rules ()
      [(_ f a ...) (f a ...)]))

  (define-syntax appl_
    (syntax-rules ()
      [(_ f a ...) (f a ...)])))

(module A racket
  (define return-state
    (λ (a)
      (λ (s)
        `(,a . ,s))))

  (define bind-state
    (λ (ma f)
      (λ (s)
        (match-let ((`(,v . ,s^) (ma s)))
          ((f v) s^)))))

  (define get-state
    (λ ()
      (λ (s) `(,s . ,s))))

  (define put-state
    (λ (new-s)
      (λ (s)
        `(_ . ,new-s))))

  (define replace-with-count
    (λ (x ls)
      (match ls
        ['() (return-state '())]
        [`(,a . ,d)
         (cond
           [(eqv? a x)
            (bind-state
             (get-state)
             (λ (s) 
               (bind-state
                (put-state (add1 s))
                (λ (d)
                  (bind-state
                   (replace-with-count x d)
                   (λ (_)                                                   
                     (return-state (cons s d))))))))]
           [else
            (bind-state
             (replace-with-count x d)
             (λ (d)
               (return-state (cons a d))))])]))))

(module B racket
  (require (submod ".." CPS-helpers))

  (define (return-state a)
    (λ (s)
      `(,a . ,s)))

  (define (bind-state ma f)
    (λ (s)
      (match-let ((`(,v . ,s^) (ma s)))
        ((f v) s^))))

  (define (get-state)
    (λ (s)
      `(,s . ,s)))

  (define (put-state new-s)
    (λ (s)
      `(_ . ,new-s)))

  (define replace-with-count
    (λ_ (x ls k)
      (match ls
        ['() (λ‾ (k) (appl_ k (λ_ (n) (appl_ n '()))))]
        [`(,a . ,d)
         (cond
           [(eqv? a x)
            (bind-state
             (get-state)
             (λ (s) 
               (bind-state
                (put-state (add1 s))
                (λ (d)
                  (bind-state
                   (replace-with-count x d)
                   (λ (_)                                                   
                     (return-state (cons s d))))))))]
           [else
            (bind-state
             (replace-with-count x d)
             (λ (d)
               (return-state (cons a d))))])]))))



