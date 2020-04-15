#lang racket
;; inline-fn call

;; back off the existence of dk;
;; remove the dk from the program, only add it in after CPSing, and
;; see what happens when you do that.

;; re-introduce dk, as the 4th and final element.
;; is disj serious or simple?
;; is fail serious or simple?
;; let us try treating it as serious, and see where that gets us. 

(define ((ee g s) dk)
  (match g
    [`(ore ,g1 ,g2)  ((ee g1 s) (λ (c) ((ee g2 s) (λ (c2) ((disj c c2) dk)))))]
    ;; pass along?
    ;; apply to it?
    ;; both? 
    [`(ande ,g1 ,g2) ((ee g1 s) (λ (c) ((bind c g2) dk)))]
    [`(succeed)      ((unit s) dk)]
    [`(fail)         ((fail) dk)]
    [`(alwayso)      ((mdelay (ee `(ore (succeed) (alwayso)) s)) dk)]
    ))

(define-syntax-rule (mdelay c)
  (λ (dk)
    (λ (sk fk)
      (dk c))))

(define ((unit s) dk)
  (λ (sk fk) 
    (sk s fk)))

(define ((fail) dk)
  (λ (sk fk) 
    (fk)))

(define ((disj c c2) dk)
  (λ (sk fk) 
    ((c dk)  ;; for the time being
     ;; (λ (c) (dk (disj c2 c)))
     sk
     (λ () ((c2 dk) sk fk))))) ;; dk

(define ((bind c g) dk)
  (λ (sk fk) ;; dk
    ((c dk) 
     ;; (λ (c) (dk (bind c g)))
     (λ (s fk) (λ (dk) (((ee g s) dk) sk fk))) ;; note this has changed the dk we are taking in here!
     ;; how different is it?
     ;; we used to not take it from the site of the other, but from this one. 
     fk)))

;; and possibly the original continuations too. 
(define kons (λ (s fk) (cons s (fk))))
(define nill (λ () '()))
(define identity (λ (c) c))

(define (looper n c)
  (match n
    [`Z ((c identity) kons nill)]       ;; identity
    [`(S ,n^) ((c identity) kons nill)]))        ;; (λ (c) (looper n^ c))

;; Could instead nest up computations here

(looper '(S Z) (ee '(ore (ande (succeed) (succeed)) (fail)) '()))

(looper '(S Z) (ee '(alwayso) '()))
(looper '(S (S (S (S (S (S (S (S (S (S Z)))))))))) (ee '(ore (alwayso) (ore (alwayso) (succeed))) '()))
(looper '(S (S (S (S (S (S (S (S (S (S Z)))))))))) (ee '(ande (ore (alwayso) (succeed)) (succeed)) '()))
(looper '(S (S (S (S (S (S (S (S (S (S Z)))))))))) (ee '(ande (ore (alwayso) (succeed)) (fail)) '()))

