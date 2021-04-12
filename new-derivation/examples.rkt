#lang racket
(require rackunit)

#| 

This file should contain a sufficient set of examples to demonstrate
failures with any number of various potential models for adding delays
and interleaving to a multi-continuation based microKanren. 

Each example should be prefixed by a description of the putative 
implementation of delays and return it's supposed to match. 

I expect each to show different behavior between a stream-based
implementation and that continuation implementation.

The key choices are freeze (a lower-level, sub-define-relation
primitive I've created for testing) and loop* (another lower-level
primitive, below run, I've created for testing).


;; first idea: The same way sk doesn't use the fk, and the way fk
;; doesn't use the sk, then dk shouldn't use either of those two. 
;; So we will just return the action to later compute, and we'll
;; always continue with the same basic sk and fk, but now in another
;; deeper part of the tree.

  (define-syntax-rule (freeze e) 
      (λ (dk)
        (λ (sk)
          (λ (fk)
            (dk e)))))

  (define loop*
    (λ (c) 
      (((c loop*) kons) nill)))

This model breaks because you lose some of the search tree upon
delaying.

;; second idea: dk should delay the computation, but make sure and
;; start right back up with the same old success and failure
;; continuations

  (define-syntax-rule (freeze e) 
      (λ (dk)
        (λ (sk)
          (λ (fk)
            (((dk e) sk) fk)))))

  (define loop*
    (λ (c) (c loop*)))

This too fails, and I think I have a world's smallest, simplest
program to demonstrate why it does.

|# 


;; Any one of these 4
;; (require (submod "mk-streams-derivation.rkt" streams-unit-map-join))
;; (require (submod "mk-streams-derivation.rkt" streams-bind-return))
(require (submod "mk-streams-derivation.rkt" sk/fk-unit-map-join))
;; (require (submod "mk-streams-derivation.rkt" sk/fk-bind-return))

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

(run (unit 5))
  (define-relation (d n)
    (unit n))

(run (d 5))

(module+ test 
(test-begin 
  (define-relation (a n)
    (b n))
  (define-relation (b n)
    (c n))
  (define-relation (c n)
    (d n))
  (define-relation (d n)
    (unit n))

  (test-equal? "get answers with delay" (run (d 5)) '(5))
  (test-equal? "get answers through several delays" (run (a 5)) '(5))

  (test-begin 
    (define-relation (e n)
      (f n))
    (define-relation (f n)
      (g n))
    (define-relation (g n)
      (e n))

    (test-equal? "unproductive relation terminates on finite fuel" (run 7 (e 5)) '())
    (test-equal? "interleaving behaves correctly, get answers" (run 500 (mplus (e 5) (a 5))) '(5))
    )
  
  (define-relation (h n)
    (i n))
  (define-relation (i n)
    (j n))
  (define-relation (j n)
    (mplus (h n) (a n)))

  (test-equal? "interleave through mplus w/1-answer works" (run 20 (h 5)) '(5 5))

  (define-relation (l n)
    (m n))
  (define-relation (m n)
    (mplus (l n) (unit n)))

  (test-equal? "interleave through mplus with multiple answers"
    (run 7 (l 5)) '(5 5 5))
  )

(test-begin 
  (define-relation (m n)
    (mplus (unit n) (m n)))

  (test-equal? "without needing interleaving, mplus gets an answer through delays"
               (run 1 (m 5)) '(5))
  (test-equal? "mplus gets the expected number of answers for amnt of fuel"
               (length (run 50 (mplus (m 5) (m 6)))) 49)
  (test-equal? "run throuh mplus gets expected number and variety of answers"
               (run 5 (mplus (m 5) (m 6))) '(5 6 5 6))
  )

(test-begin 
 (define-relation (l n)
   (mplus (unit n) (mplus (unit n) (l n))))

 (test-equal? "Test indicates that we are measuring the number of pulls, not the number of answers."
              (length (run 50 (mplus (l 5) (l 6)))) 98)

 (define-relation (unproductiveo x)
   (unproductiveo x))

 (test-equal? "unproductive relation is unproductive even w/ lots of fuel"
  (run 50 (unproductiveo 5)) '()))

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

(test-begin 
 ;; A giant delay free program to test the order of answers coming out.
 ;; uniquely decomposable, so we should be able to tell what came from where and precisely _how_ :-)

  (test-equal? "giant delay-free program for order of answers tests"
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
   '(14300
     9797
     103
     107
     32300
     230
     290
     28028
     197664197
     212437853
     63308
     322
     406
     32175
     63063
     72675
     345
     435
     142443
     483
     609
     165408100578763903439517733077727
     211872661518177380736555833924947
     176812278728405769807080001373807
     226480371591249076047320958929827
     213305648807259742183279259921263
     273225043825078501041769358945443
     239535258551436034828672477002247
     306822776993143400256529146456667
     146286492986972112127
     187379629563856943347
     156372318415548184207
     200298650274953084227
     188646959786991614863
     241639516550460735043
     211844358272716580647
     271353264185981859067
     681203728600791516219761331692287
     872559726577778208828745787621107
     296868658084609489807
     380261622554362805827
     728169800092946059136378027930767
     932718972305634218630210593432387
     317336477084984749087
     406479028303586325907
     878461228867840596469551455893903
     1125228558634773449994897972028483
     986483193299796045177769857242407
     1263594823808842272538267025394427
     382833497886185578783
     490374726701101590163
     429909481587048675127
     550674759924315686347))


 
 )
(test-begin
;; Really simple programs to debug and work through.

  (check-equal? 
   (run ((bind
          (freeze (return 2)))
         (lambda (a)
           (return (* a 5)))))
   '(10))

;;; could not be simpler program
  (check-equal? 
   (run ((bind
          (freeze (mzero)))
         (lambda (a) (mzero))))
   '())
)

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

;; ((((λ (fk)
;;      (fk))
;;    nill)
;;   kons) 
;;  nill)

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
)
