#lang racket

#| 

This is/was an attempt to derive a sk/fk implementation of a function
from the CPSed version of that same function by either doubly CPSing,
or duplicating the original continuation and then
specializing. Neither of these approaches seemed successful


|# 


;; Original program
(define (rf x l)
  (match l
    [`() l]
    [`(,a . ,d)
     (if (eqv? a x)
         d
         (cons (car a) (rf x d)))]))

(rf 'a '(b d c a b d a))

;; straghtforward CPS it
(define (rf-cps x l k)
  (match l
    [`() (k l)]
    [`(,a . ,d)
     (if (eqv? a x)
         (k d)
         (rf x d
             (lambda (l2)
               (k (cons (car a) l2)))))]))

(rf-cps 'a '(b d c a b d a) (lambda (v) v))

;; Don't CPS again, that seems like possibly the wrong direction? 
;; (define (rf-cps-cps x l sk k)
;;   (match l
;;     [`() (sk l k)]
;;     [`(,a . ,d)
;;      (if (eqv? a x)
;;          (sk d k)
;;          (rf-cps-cps x d
;;                      (lambda (l2 k)
;;                        (k (cons (car a) l2)))
;;                      k))]))

;; (rf-cps-cps 'a '(b d c a b d a) (lambda (v k) (k v)) (lambda (v) v))


;; just pass the same k along 2x, duplicatively?
(define (rf-cps-2x x l k k2)
  (match l
    [`() (k l)] ;; never use the 2nd one; just sits there
    [`(,a . ,d)
     (if (eqv? a x)
         (k d)  ;; never use the second one, just sits there
         (rf-cps-2x x d
                    (lambda (l2)
                      (k (cons (car a) l2)))
                    (lambda (l2)
                      (k2 (cons (car a) l2)))))]))

(rf-cps-2x 'a '(b d c a b d a) (lambda (v) v) (lambda (v) v))

;; Duplicate the original continuation. Then, use a different
;; continuations for each of the "built-outside-of-a-k" tail calls
(define (rf-cps-2x-both x l k k2)
  (match l
    [`() (k2 l)]
    [`(,a . ,d)
     (if (eqv? a x)
         (k d)
         (rf-cps-2x-both x d
                    (lambda (l2)
                      (k (cons (car a) l2)))
                    (lambda (l2)
                      (k2 (cons (car a) l2)))))]))

(rf-cps-2x-both 'a '(b d c a b d a) (lambda (v) v) (lambda (v) v))


;; Extend the functionality

;; Use a different continuations for each of the "built-outside-of-a-k" tail calls
;; Doesn't run ri8ght now b/c we don't have `sk` as a named continuatino 
(define (rf-cps-2x-both-part-2 x l k k2)
  (match l
    [`() (k2 l)]
    [`(,a . ,d) #:when (pair? a)
                (rf-cps-2x-both-part-2 x a
                     (lambda (l)
                       (sk (cons l d)))
                     (lambda (l)
                       (rf-cps-2x-both-part-2 x d
                            (lambda (l2)
                              (sk (cons a l2)))
                            fk)))]
    [`(,d . ,a)
     (if (eqv? a x)
         (k d)
         (rf-cps-2x-both-part-2 x d
                    (lambda (l2)
                      (k (cons (car a) l2)))
                    (lambda (l2)
                      (k2 (cons (car a) l2)))))]))

(rf-cps-2x-both-part-2 'a '(b d c a b d a) (lambda (v) v) (lambda (v) v))

#| 

Other implementation approaches. Some of these might be other
interesting places to start.

(define (drf x l*)
  (match l*
    [`() l*]
    [`(,a . ,d) #:when (pair? a)
     (if (not (equal? (drf x a) a))
         (cons (drf x a) d)
         (drf x d))]
    [`(,a . ,d)
     (if (eqv? a x)
         d
         (cons (car a) (drf x d)))]))

(define (drf x l* sk fk)
  (match l*
    [`() (fk l*)]
    [`(,a . ,d) #:when (pair? a)
                (drf x a
                     (lambda (l)
                       (sk (cons l d)))
                     (lambda (l)
                       (drf x d
                            (lambda (l2)
                              (sk (cons a l2)))
                            fk)))]
    [`(,a . ,d)
     (if (eqv? a x)
         (sk d)
         (drf x d
              (lambda (l)
                (sk (cons a l)))
              fk))]))



(define (drf x l* sk fk)
  (match l*
    [`() (fk)]
    [`(,a . ,d) #:when (pair? a)
                (drf x a
                     (lambda (l)
                       (sk (cons l d)))
                     (lambda ()
                       (drf x d
                            (lambda (l2)
                              (sk (cons a l2)))
                            fk)))]
    [`(,a . ,d)
     (if (eqv? a x)
         (sk d)
         (drf x d
              (lambda (l)
                (sk (cons a l)))
              fk))]))

;; or the combinatorized version,
(define (drf x l* sk fk)
  (match l*
    [`() (fk)]
    [`(,a . ,d) #:when (pair? a)
     (drf x a 
          (compose sk ((curryr cons) d))
          (λ ()
            (drf x d
                 (compose sk ((curry cons) a))
                 fk)))]
    [`(,a . ,d)
     (if (eqv? a x)
	 (sk d)
         (drf x d (compose sk ((curry cons) a))
              fk))]))

(define (drff x l*)
  (drf x l* (λ (l) l) (λ () l*)))
|# 
