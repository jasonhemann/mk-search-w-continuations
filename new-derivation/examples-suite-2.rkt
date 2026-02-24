#lang racket
(require rackunit)

;; Any one of these 4
(require (submod "mk-streams-derivation.rkt" streams-unit-map-join))
;; (require (submod "mk-streams-derivation.rkt" streams-bind-return))
;; (require (submod "mk-streams-derivation.rkt" sk/fk-unit-map-join))
;; (require (submod "mk-streams-derivation.rkt" sk/fk-bind-return))


(define-relation (t n)
  (unit n))

(define-relation (penultimate n)
  (mzero))

(check-equal? (run 10 (mzero)) '())
(check-equal? (run 10 (mplus (mzero) (mzero))) '())
(check-equal? (run 10 (unit 5)) '(5))
(check-equal? (run 10 (penultimate 5)) '())
(check-equal? (run 10 (t 5)) '(5))
(check-equal? (run 10 (mplus (unit 5) (mzero))) '(5))
(check-equal? (run 10 (mplus (mzero) (t 5))) '(5))

;; (run 10 (mplus (penultimate 5) (mzero)))
;; (run 10 (mplus (t 5) (mzero)))
;; (run 10 (mplus (t 5) (t 6)))

;; (((dk g) sk) fk)

;; (mzero)

;; ((((λ (c1^) (loop* (mplus m2 c1^)))
;;    (mzero))
;;   kons)
;;  nill)

;; ((((mplus (lambda (dk)
;;             (lambda (sk)
;;               (lambda (fk)
;;                 (((dk (mzero)) sk) fk))))
;;           (mzero))
;;    loop*)
;;   kons)
;;  nill)

(define mpluts
  (lambda (m1 m2)
    (lambda (dk)
      (lambda (sk)
        (lambda (fk)
          (((m1
             (λ (c1^)
               (loop* (mpluts m2 c1^))))
            sk)
           (lambda ()
             (((m2 dk) sk) fk))))))))

(define-relation (reln x)
  (mzero))

(run (mpluts (reln 5) (mzero)))

(pretty-print "here is not a problem")

;; (define mpluts
;;   (lambda (m1 m2)
;;     (lambda (dk)
;;       (lambda (sk)
;;         (lambda (fk)
;;           (((m1
;;              (λ (c1^)
;;                (loop* (mpluts m2 c1^))))
;;             sk)
;;            (lambda ()
;;              (((m2 dk) sk) fk))))))))

;; (define-relation (reln x)
;;   (mzero))

;; ((((mpluts
;;     (reln 5)
;;     (mzero))
;;    loop*)
;;   kons)
;;  nill)


;; Quarantined scratch reductions: these are kept for derivation notes
;; but intentionally disabled because they rely on sk/fk-specific names
;; (`kons`/`nill`) that are not in scope for the active stream module.
#|
(((((lambda (m1 m2)
      (lambda (dk)
        (lambda (sk)
          (lambda (fk)
            (((m1
               (λ (c1^)
                 (loop* (mplus m2 c1^))))
              sk)
             (lambda ()
               (((m2 dk) sk) fk)))))))
    (lambda (dk)
      (lambda (sk)
        (lambda (fk)
          (((dk (mzero)) sk) fk))))
    (mzero))
   loop*)
  kons)
 nill)

((((lambda (dk)
     (lambda (sk)
       (lambda (fk)
         (((dk
            (mzero))
           sk)
          fk))))
   (λ (c1^)
     (loop* (mplus (mzero) c1^))))
  kons)
 (lambda ()
   ((((mzero) loop*) kons) nill)))

((((λ (c1^)
     (loop* (mplus (mzero) c1^)))
   (mzero))
  kons)
 (lambda ()
   ((((mzero) loop*) kons) nill)))

((((λ (c1^) (loop* (mplus (mzero) c1^)))
   (mzero))
  kons)
 nill)

((((λ (c1^) (loop* (mplus (mzero) c1^)))
   (mzero))
  kons)
 nill)

(((loop* (mplus (mzero) (mzero)))
  kons)
 nill)

((((mplus (mzero) (mzero)) loop*)
  kons)
 nill)
|#


(define-relation (g n)
  (mzero))

(check-equal?
   (run (mplus
         (mplus
          (g 1)
          (unit 'u))
         (mzero)))
   '(u))


;; Well, this doesn't feel like it's going to work as it is. It feels
;; like something that's going to need another k passed around inside
;; the original one.

;; Clearly I found what went wrong with the earlier option in bind, we
;; weren't passing the correct k through and dropping part of the
;; continuation. But with this version where we pass it all along, it
;; ends up being that we didn't do the appropriate thing with the
;; other continuations, and we ended up threading/using them twice,
;; specifically it looks like, the success continuations.


(run
    (mplus 
     (λ (dk)
       (λ (sk)
         (λ (fk)
           (((dk 
              (mplus 
               (lambda (dk)
                 (lambda (sk)
                   (lambda (fk)
                     ((sk 'u) fk))))
               (mzero))) 
             sk)
            (λ ()
              ((sk 'u) fk))))))
     (mzero)))
'(u u)
