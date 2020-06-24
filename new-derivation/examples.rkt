#lang racket
(require rackunit)

;; Any one of these 4
;; (require (submod "mk-streams-derivation.rkt" streams-unit-map-join))
;; (require (submod "mk-streams-derivation.rkt" streams-bind-return))
;; (require (submod "mk-streams-derivation.rkt" sk/fk-unit-map-join))
(require (submod "mk-streams-derivation.rkt" sk/fk-bind-return))

#| 

This should be the file in which we /use/ these various
implementations, and to show that they all work the same or
independent of one another, to get the right answers out.
(loop* 
(lambda (dk)
  (lambda (sk)
    (lambda (fk)
      ((((lambda (dk)
           (lambda (sk)
             (lambda (fk)
               ((sk 6) 
                (lambda () 
                  ((sk 5) fk))))))
         (lambda (m^)
           (dk ((bind m^) 
                (lambda (n)
                  (lambda (dk)
                    (lambda (sk)
                      (lambda (fk)
                        (dk 
                         (lambda (dk)
                           (lambda (sk)
                             (lambda (fk)
                               ((sk n) fk)))))))))))))
        (lambda (b)
          (lambda (fk)
            (dk 
             (lambda (dk)
               (lambda (sk)
                 (lambda (fk)
                   ((sk b) fk))))))))
       fk)))))
|#

(test-begin 
  (define-relation (a n)
    (b n))
  (define-relation (b n)
    (c n))
  (define-relation (c n)
    (d n))
  (define-relation (d n)
    (unit n))

  (check-equal? (run (d 5)) '(5))
  (check-equal? (run (a 5)) '(5))

  (test-begin 
    (define-relation (e n)
      (f n))
    (define-relation (f n)
      (g n))
    (define-relation (g n)
      (e n))

    (check-equal? (run 7 (e 5)) '())
    (check-equal? (run 500 (mplus (e 5) (a 5))) '(5))
    )
  
  (define-relation (h n)
    (i n))
  (define-relation (i n)
    (j n))
  (define-relation (j n)
    (mplus (h n) (a n)))

  (check-equal? (run 20 (h 5)) '(5 5))

  (define-relation (l n)
    (m n))
  (define-relation (m n)
    (mplus (l n) (unit n)))

  (check-equal? (run 7 (l 5)) '(5 5 5))
  )

(test-begin 
  (define-relation (m n)
    (mplus (unit n) (m n)))

  (check-equal? (run 1 (m 5)) '(5))
  (check-equal? (length (run 50 (mplus (m 5) (m 6)))) 49)
  (check-equal? (run 5 (mplus (m 5) (m 6))) '(5 6 5 6))
  )

(test-begin 
 (define-relation (l n)
   (mplus (unit n) (mplus (unit n) (l n))))

 ;; This indicates that we are measuring the number of pulls, not the number of answers.
 (check-equal? (length (run 50 (mplus (l 5) (l 6)))) 98)

 (define-relation (unproductiveo x)
   (unproductiveo x))

 (check-equal? (run 50 (unproductiveo 5)) '()))

;; This seems promising, don't know if it's right.
(test-begin 
  (define-relation (a n)
    (mplus (unit 'a) (b (add1 n))))

  (define-relation (b n)
    (mplus (c (add1 n)) (unit 'b)))

  (define-relation (c n)
    (unit 'c))

  (define-relation (d n)
    (mplus (e (add1 n)) (unit 'd)))

  (define-relation (e n)
    (mplus (unit 'e) (f (add1 n))))

  (define-relation (f n)
    (unit 'f))

  (define-relation (g n)
    (mzero))
  
  (check-equal?
   (run (mplus
         (mplus
          (g 1)
          (unit 'u))
         (mzero)))
   '(u))
  
  (check-equal?
   (run (mplus
         (mplus
          (mplus
           (c 1)
           (unit 'u))
          (mplus
           (e 1)
           (mplus
            (a 1)
            (unit 'w))))
         (mplus
          (mplus
           (unit 'v)
           (f 1))
          (mplus
           (b 1)
           (d 1)))))
   '(v u c f e w a f b c d e b c f)))

(test-begin 
  (check-equal? (run ((bind (mzero)) (λ (n) (mzero)))) '())
  (check-equal? (run ((bind (mzero)) (λ (n) (unit 5)))) '())

  (check-equal? (run ((bind (unit 5)) (λ (n) (mzero))))   '())
  (check-equal? (run ((bind (unit 5)) (λ (n) (unit (* n n))))) '(25))

  (test-begin 
    (define-relation (a n)
      (mplus (unit n) (b (add1 n))))

    (define-relation (b n)
      (mplus (unit n) (c (add1 n))))

    (define-relation (c n)
      (mplus (unit n) (d (add1 n))))

    (define-relation (d n)
      (unit n))

    (define-relation (t n)
      (mplus (unit (add1 n)) (unit n)))

    (check-equal? (run ((bind (t 5)) (λ (n) (t (* n n)))))
                  '(37 36 26 25))
    
    (check-equal? (run ((bind (c 5)) (λ (n) (c (* n n)))))
                  '(25 26 36 37))

    (check-equal? (run ((bind (a 5)) (λ (n) (a (* n n)))))
                  '(25 26 27 36 28 37 38 49 39 50 64 51 65 52 66 67)))
  
  (define-relation (nearly n)
    (unit n))

  (check-equal? (run ((bind (nearly 5)) (λ (n) (mzero)))) '())
  (check-equal? (run ((bind (nearly 5)) (λ (n) (unit (* n n))))) '(25))
  )

;; ;; Experimenting version
;; (loop* 
;;  (lambda (dk)
;;    (lambda (sk)
;;      (lambda (fk)
;;        ((((lambda (dk)
;;             (lambda (sk)
;;               (lambda (fk)
;;                 ((sk 6) 
;;                  (lambda () 
;;                    ((sk 5) fk))))))
;;           (lambda (m^)
;;             (dk ((bind m^) 
;;                  (lambda (n)
;;                    (lambda (dk)
;;                      (lambda (sk)
;;                        (lambda (fk)
;;                          (dk 
;;                           (lambda (dk)
;;                             (lambda (sk)
;;                               (lambda (fk)
;;                                 ((sk n) fk)))))))))))))
;;          (lambda (b)
;;            (lambda (fk)
;;              (lambda (m^)
;;                (mplus ))
;;              (dk

              

;;               ;; merely invoking the current dk must be a mistake here, since this is where we normally do the interleave
;;               ;; Right now we haven't been.
;;               ;; so we need to have an mplus here b/t these things
;;               (lambda (dk) 
;;                 (lambda (_)
;;                   (lambda (_)
;;                     ((sk b) fk))))))))
;;         fk)))))



;; ;; Okay, so what if instead of passing the three continuations in to continue,
;; ;; instead we only passed in (one) continuation to continue,
;; ;; and kept the other two around?
;; ;; e.g.
;; ;; (define loop* (lambda (c) (c loop*)))
;; ;;

;; ;; OG saved version
;; (loop* 
;;  (lambda (dk)
;;    (lambda (sk)
;;      (lambda (fk)
;;        ((((lambda (dk)
;;             (lambda (sk)
;;               (lambda (fk)
;;                 ((sk 6) 
;;                  (lambda () 
;;                    ((sk 5) fk))))))
;;           (lambda (m^)
;;             (dk ((bind m^) 
;;                  (lambda (n)
;;                    (lambda (dk)
;;                      (lambda (sk)
;;                        (lambda (fk)
;;                          (dk 
;;                           (lambda (dk)
;;                             (lambda (sk)
;;                               (lambda (fk)
;;                                 ((sk n) fk)))))))))))))
;;          (lambda (b)
;;            (lambda (fk)
             
;;              ;; I have (f b) right here
;;              ;; So what I need to do
;;              ;; is to impose an mplus
;;              ;; I don't have the second option to mplus, all I have is the failure continuation

;;              (mplus 
;;               (lambda (dk)
;;                 (lambda (sk)
;;                   (lambda (fk)
;;                     (f b)
                    
;;                     ))) 
;;                     )

;;              (dk  ;; So this is right. This has to be right; we must
;;               ;; delay the computation itself.  When we invoke again
;;               ;; we have to continue, at least in this case, with the
;;               ;; saved failure continuation; the one we took in,
;;               ;; rather than the one we ate. 
;;               (lambda (dk)
;;                 (lambda (_)
;;                   (lambda (_)
;;                     ((sk b) fk))))))))
;;         fk)))))


;; So, the loop idea here is obviously wrong. Instead, it's a
;; complicated interplay where things are passed back and forth in a
;; single flow, and in the special shortcut case there's a soft-out
;; there.



;; (define-relation (t n)
;;   (unit (* n n)))
;; (((((bind (mplus (unit 6) (unit 5))) t) loop*) kons) nill)

;;     (define-relation (t n)
;;       (unit (* n n)))
;; (run (join ((map (λ (n) (t n))) (mplus (unit 6) (unit 5)))))
;; '(36 37 25 26)



;; (define-relation (t n)
;;   (return 42))
;; (((((bind ((bind (return 6)) t)) t) loop*) kons) nill)

;; Works
;; (define-relation (t n)
;;   (return n))
;; (loop* (mplus (t 6) ((bind (return 5)) t)))

;; Broken
;; (define-relation (t n)
;;   (return n))
;; (loop* ((bind (mplus (return 6) (return 5))) t))

;; Experiment with a different dk

;; (define loop2
;;   (lambda (c)
;;     (lambda (sk)
;;       (lambda (fk)
;; 	(((c loop2)
;; 	  sk)
;; 	 fk)))))
;;
;;
;; Created two manual delays around a return, to make sure what would happen
;; Now, to consider how to deal with that in the general case. 
;; (((
;;    (lambda (dk)
;;      (lambda (sk)
;;        (lambda (fk)
;;          (((dk (lambda (dk)
;;                  (lambda (sk)
;;                    (lambda (fk)
;;                      (((dk (lambda (dk)
;;                              (lambda (sk)
;;                                (lambda (fk)
;;                                  ((sk 5) fk)))))
;;                        sk)
;;                       fk)))))
;;            sk)
;;           fk))))
;;    loop2)
;;   kons)
;;  nill)
;; '(5)


;; (define-relation (t n) (return (* n n)))
;; (force (force ((bind (mplus (return 6) (return 5))) t)))
;; >(return 6)
;; <'(6)
;; >(return 5)
;; <'(5)
;; >(mplus '(6) '(5))
;; > (mplus '() '(5))
;; < '(5)
;; <'(6 5)
;; >(bind '(6 5))
;; <#<procedure:...s-derivation.rkt:118:2>
;; >(bind '(5))
;; <#<procedure:...s-derivation.rkt:118:2>
;; >(bind '())
;; <#<procedure:...s-derivation.rkt:118:2>
;; >(mplus #<promise:t> '())
;; <#<promise:mplus>
;; >(mplus #<promise:t> #<promise:mplus>)
;; <#<promise:mplus>
;; >(return 36)
;; <'(36)
;; >(mplus #<promise:mplus> '(36))
;; <#<promise:mplus>
;; >(return 25)
;; <'(25)
;; >(mplus '() '(25))
;; <'(25)
;; >(mplus '(36) '(25))
;; > (mplus '() '(25))
;; < '(25)
;; <'(36 25)
;; '(36 25)

;; The delay free program to test the order of answers coming out.
;; uniquely decomposable, so we should be able to tell what came from where and precisely _how_ :-)
(run (freeze (mplus
              (freeze ((bind
                        (freeze ((bind
                                  (freeze (mplus
                                           (freeze (return 2))
                                           (freeze (return 3))
                                           )))
                                 (lambda (a)
                                   (freeze (mplus
                                            (freeze (return (* a 5)))
                                            (freeze (return (* a 7)))
                                            ))))))
                       (lambda (b)
                         (freeze (mplus
                                  (freeze ((bind
                                            (freeze (return (* b 11))))
                                           (lambda (c)
                                             (freeze (return (* b c 13)))
                                             )))
                                  (freeze (mplus
                                           (freeze ((bind
                                                     (freeze (return (* b 17))))
                                                    (lambda (d)
                                                      (freeze (return (* b d 19)))
                                                      )))
                                           (freeze (mplus
                                                    (freeze (return (* b 23)))
                                                    (freeze (return (* b 29)))
                                                    )))))))))
              (freeze (mplus
                       (freeze ((bind
                                 (freeze ((bind
                                           (freeze (mplus
                                                    (freeze (return 31))
                                                    (freeze (return 37))
                                                    )))
                                          (lambda (e)
                                            (freeze (mplus
                                                     (freeze ((bind
                                                               (freeze (return (* e 41))))
                                                              (lambda (f)
                                                                (freeze (return (* e f 43)))
                                                                )))
                                                     (freeze ((bind
                                                               (freeze (return (* e 47))))
                                                              (lambda (g)
                                                                (freeze (return (* e 53)))
                                                                )))))))))
                                (lambda (h)
                                  (freeze ((bind
                                            (freeze (mplus
                                                     (freeze (mplus
                                                              (freeze (return (* h 59)))
                                                              (freeze (return (* h 61)))
                                                              ))
                                                     (freeze (mplus
                                                              (freeze (return (* h 67)))
                                                              (freeze (return (* h 71)))
                                                              )))))
                                           (lambda (i)
                                             (freeze (mplus
                                                      (freeze ((bind
                                                                (freeze (return (* h i 73))))
                                                               (lambda (j)
                                                                 (freeze (return (* h i j 79)))
                                                                 )))
                                                      (freeze ((bind
                                                                (freeze (return (* h i 83))))
                                                               (lambda (k)
                                                                 (freeze (return (* h i k 89)))
                                                                 )))))))))))
                       (freeze (mplus
                                (freeze (mplus
                                         (freeze ((bind
                                                   (freeze (return 97)))
                                                  (lambda (l)
                                                    (freeze (return (* l 101)))
                                                    )))
                                         (freeze (mplus
                                                  (freeze (return 103))
                                                  (freeze (return 107))
                                                  ))))
                                (freeze ((bind
                                          (freeze (mplus
                                                   (freeze (return 109))
                                                   (freeze (return 113))
                                                   )))
                                         (lambda (m)
                                           (freeze ((bind
                                                     (freeze (return (* m 127))))
                                                    (lambda (n)
                                                      (freeze (return (* m n 131)))
                                                      )))))))))))))


;; A simple enough program to debug and work through. 

(run ((bind
       (freeze (return 2)))
      (lambda (a)
        (return (* a 5)))))

;;; could not be simpler program
(run ((bind
       (freeze (mzero)))
      (lambda (a) (mzero))))


;; (run (letrec 
;;          ((bind (lambda (m) 
;;                   (lambda (f)
;;                     (λ (dk)
;;                       (λ (sk)
;;                         (λ (fk)
;;                           (((m
;;                              (λ (m^)
;;                                (((dk ((bind m^) f))
;;                                  sk)
;;                                 fk)))
;;                             (λ (b)
;;                               (λ (fk)
;;                                 ((((f b)
;;                                    (lambda (m^)
;;                                      (((dk m^)
;;                                        sk)
;;                                       fk)))
;;                                   sk)
;;                                  fk))))
;;                            fk)))))
;;                   )))
;;        (λ (dk)
;;          (λ (sk)
;;            (λ (fk)
;;              ((((λ (dk)
;;                   (λ (sk)
;;                     (λ (fk)
;;                       (((dk (mzero)) sk) fk))))
;;                 (λ (m^)
;;                   (((dk ((bind m^) 
;;                          (lambda (a) (mzero))))
;;                     sk)
;;                    fk)))
;;                (λ (b)
;;                  (λ (fk)
;;                    ((((mzero)
;;                       (lambda (m^)
;;                         (((dk m^)
;;                           sk)
;;                          fk)))
;;                      sk)
;;                     fk))))
;;               fk))))))


;; (run (letrec 
;;          ((bind (lambda (m) 
;;                   (lambda (f)
;;                     (λ (dk)
;;                       (λ (sk)
;;                         (λ (fk)
;;                           (((m
;;                              (λ (m^)
;;                                (((dk ((bind m^) f))
;;                                  sk)
;;                                 fk)))
;;                             (λ (b)
;;                               (λ (fk)
;;                                 ((((f b)
;;                                    (lambda (m^)
;;                                      (((dk m^)
;;                                        sk)
;;                                       fk)))
;;                                   sk)
;;                                  fk))))
;;                            fk))))))))
;;        (λ (dk)
;;          (λ (sk)
;;            (λ (fk)
;;              ((((λ (dk)
;;                   (λ (sk)
;;                     (λ (fk)
;;                       (((dk (λ (dk)
;;                               (λ (sk)
;;                                 (λ (fk)
;;                                   (fk))))) 
;;                         sk) 
;;                        fk))))
;;                 (λ (m^)
;;                   (((dk ((bind m^) 
;;                          (lambda (a) 
;;                            (λ (dk)
;;                              (λ (sk)
;;                                (λ (fk)
;;                                  (fk)))))))
;;                     sk)
;;                    fk)))
;;                (λ (b)
;;                  (λ (fk)
;;                    ((((λ (dk)
;;                         (λ (sk)
;;                           (λ (fk)
;;                             (fk))))
;;                       (lambda (m^)
;;                         (((dk m^)
;;                           sk)
;;                          fk)))
;;                      sk)
;;                     fk))))
;;               fk))))))



;; (run (letrec 
;;          ((bind (lambda (m) 
;;                   (lambda (f)
;;                     (λ (dk)
;;                       (λ (sk)
;;                         (λ (fk)
;;                           (((m
;;                              (λ (m^)
;;                                (((dk ((bind m^) f))
;;                                  sk)
;;                                 fk)))
;;                             (λ (b)
;;                               (λ (fk)
;;                                 ((((f b)
;;                                    (lambda (m^)
;;                                      (((dk m^)
;;                                        sk)
;;                                       fk)))
;;                                   sk)
;;                                  fk))))
;;                            fk))))))))
;;        (λ (dk)
;;          (λ (sk)
;;            (λ (fk)
;;              (((((dk 
;;                   (((lambda (m) 
;;                       (lambda (f)
;;                         (λ (dk)
;;                           (λ (sk)
;;                             (λ (fk)
;;                               (((m
;;                                  (λ (m^)
;;                                    (((dk ((bind m^) f))
;;                                      sk)
;;                                     fk)))
;;                                 (λ (b)
;;                                   (λ (fk)
;;                                     ((((f b)
;;                                        (lambda (m^)
;;                                          (((dk m^)
;;                                            sk)
;;                                           fk)))
;;                                       sk)
;;                                      fk))))
;;                                fk)))))) 
;;                     (λ (dk)
;;                       (λ (sk)
;;                         (λ (fk)
;;                           (fk))))) 
;;                    (lambda (a) 
;;                      (λ (dk)
;;                        (λ (sk)
;;                          (λ (fk)
;;                            (fk)))))))
;;                  (λ (b)
;;                    (λ (fk)
;;                      (fk))))
;;                 fk) 
;;                sk) 
;;               fk))))))


;; (run (letrec 
;;          ((bind (lambda (m) 
;;                   (lambda (f)
;;                     (λ (dk)
;;                       (λ (sk)
;;                         (λ (fk)
;;                           (((m
;;                              (λ (m^)
;;                                (((dk ((bind m^) f))
;;                                  sk)
;;                                 fk)))
;;                             (λ (b)
;;                               (λ (fk)
;;                                 ((((f b)
;;                                    (lambda (m^)
;;                                      (((dk m^)
;;                                        sk)
;;                                       fk)))
;;                                   sk)
;;                                  fk))))
;;                            fk))))))))
;;        (λ (dk)
;;          (λ (sk)
;;            (λ (fk)
;;              (((((dk 
;;                   ((lambda (f)
;;                      (λ (dk)
;;                        (λ (sk)
;;                          (λ (fk)
;;                            (fk))))) 
;;                    (lambda (a) 
;;                      (λ (dk)
;;                        (λ (sk)
;;                          (λ (fk)
;;                            (fk)))))))
;;                  (λ (b)
;;                    (λ (fk)
;;                      (fk))))
;;                 fk) 
;;                sk) 
;;               fk))))))

;; The next two steps reveal precisely the problem. Our hypothesis was
;; that the dk needed to receive the computation being delayed, and to
;; also receive the present sk and fk. Which we pass along here.  We
;; have also the sk and fk of the original computation being computed.

;; The original dk was loop*, which then just took the c and made it
;; invoke itself, which then took the sk and fk that we passed, and
;; produce '() from it. Notice that sk and fk are still waiting to be
;; taken in as arguments. So this is where we get the failure "Hey,
;; '() isn't a function! You can't pass kons to it!"

;; This is a wonderful example from which to start, I believe. Very
;; neat place. If we didn't pass along the sk and fk here, but had the
;; originals get inherited along, this example would work out
;; correctly. But other examples did not.

;; (run (λ (dk)
;;        (λ (sk)
;;          (λ (fk)
;;            (((((dk 
;;                 (λ (dk)
;;                   (λ (sk)
;;                     (λ (fk)
;;                       (fk)))))
;;                (λ (b)
;;                  (λ (fk)
;;                    (fk))))
;;               fk) 
;;              sk) 
;;             fk)))))

;; (define kons 
;;   (λ (a)
;;     (λ (fk)
;;       (cons a (fk)))))

;; (define nill 
;;   (λ ()
;;     '()))

;; (define loop*
;;   (λ (c)
;;     (c loop*)))

;; (((((loop*
;;      (λ (dk)
;;        (λ (sk)
;;          (λ (fk)
;;            (fk)))))
;;     (λ (b)
;;       (λ (fk)
;;         (fk))))
;;    nill) 
;;   kons) 
;;  nill)


(define kons 
  (λ (a)
    (λ (fk)
      (cons a (fk)))))

(define nill 
  (λ ()
    '()))

(define loop*
  (λ (c)
    (c loop*)))



((((λ (fk)
     (fk))
   nill)
  kons) 
 nill)

;; The solution must be different, perhaps more complex than either of
;; those.

;; What do I not know?

;; * What the initial dk should be. An empty continuation?
;; * What the other continuations should do. Which one is cpsed over
;; the other?
;; * What should happen when we want to delay a computation for a
;; while?
;; * Should we think about this in full, undelimited control? Or
;; perhaps in terms of delimited control

;; So, the new plan: meditate on what I want to happen in this
;; situation, and see what comes to mind. I understand these tools,
;; techniques well enough. That should get me thinking.


;; (run (λ (dk)
;;        (λ (sk)
;;          (λ (fk)
;;            (((((dk 
;;                 (λ (dk)
;;                   (λ (sk)
;;                     (λ (fk)
;;                       (fk)))))
;;                (λ (b)
;;                  (λ (fk)
;;                    (fk))))
;;               fk) 
;;              sk) 
;;             fk)))))

;; So. What we need to do is to control the

;; We could say that we control the extent of the backtracking. via delimited

;; We could say that we parameterize the success and failures, so that
;; We know what to do when we figure out the order.


