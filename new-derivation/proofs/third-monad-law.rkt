#lang racket

(bind m (lambda (x y) (bind (g x y) h)))
= Unfolding the outer definition of bind
((fix2
  (λ (bind)
    (λ (m f)
      (λ (dk sk fk)
        (m
         (λ (m^)
           (dk (bind m^ f)))
         (λ (b vc c)
           ((mplus (f b vc) (bind c f))
            dk
            sk
            fk))
         fk)))))
 m
 (lambda (x)
   ((fix2
     (λ (bind)
       (λ (m f)
         (λ (dk sk fk)
           (m
            (λ (m^)
              (dk (bind m^ f)))
            (λ (b vc c)
              ((mplus (f b vc) (bind c f))
               dk
               sk
               fk))
            fk)))))
    (g x)
    h)))
= unfolding the fixpoint definition 1x
((λ (m f)
   (λ (dk sk fk)
     (m
      (λ (m^)
        (dk (bind m^ f)))
      (λ (b vc c)
        ((mplus (f b vc) (bind c f))
         dk
         sk
         fk))
      fk)))
 m
 (lambda (x)
   ((fix2
     (λ (bind)
       (λ (m f)
         (λ (dk sk fk)
           (m
            (λ (m^)
              (dk (bind m^ f)))
            (λ (b vc c)
              ((mplus (f b vc) (bind c f))
               dk
               sk
               fk))
            fk)))))
    (g x)
    h)))
= reducing the redex
(λ (dk sk fk)
  (m
   (λ (m^)
     (dk (bind
          m^
          (lambda (x)
            ((fix2
              (λ (bind)
                (λ (m f)
                  (λ (dk sk fk)
                    (m
                     (λ (m^)
                       (dk (bind m^ f)))
                     (λ (b vc c)
                       ((mplus (f b vc) (bind c f))
                        dk
                        sk
                        fk))
                     fk)))))
             (g x)
             h)))))
   (λ (b vc c)
     ((mplus (f b vc)
             (bind
              c
              (lambda (x)
                ((fix2
                  (λ (bind)
                    (λ (m f)
                      (λ (dk sk fk)
                        (m
                         (λ (m^)
                           (dk (bind m^ f)))
                         (λ (b vc c)
                           ((mplus (f b vc) (bind c f))
                            dk
                            sk
                            fk))
                         fk)))))
                 (g x)
                 h))))
      dk
      sk
      fk))
   fk))
= cases. Each computation must use one of the three continuations.

1. m is a computation that invokes the dk
(λ (dk sk fk)
  (m
   (λ (m^)
     (dk (bind
          m^
          (lambda (x)
            ((fix2
              (λ (bind)
                (λ (m f)
                  (λ (dk sk fk)
                    (m
                     (λ (m^)
                       (dk (bind m^ f)))
                     (λ (b vc c)
                       ((mplus (f b vc) (bind c f))
                        dk
                        sk
                        fk))
                     fk)))))
             (g x)
             h)))))
   (λ (b vc c)
     ((mplus (f b vc)
             (bind
              c
              (lambda (x)
                ((fix2
                  (λ (bind)
                    (λ (m f)
                      (λ (dk sk fk)
                        (m
                         (λ (m^)
                           (dk (bind m^ f)))
                         (λ (b vc c)
                           ((mplus (f b vc) (bind c f))
                            dk
                            sk
                            fk))
                         fk)))))
                 (g x)
                 h))))
      dk
      sk
      fk))
   fk))
= replace with the value of invoking the continuation
(λ (dk sk fk)
  (dk (bind
       m
       (lambda (x)
         ((fix2
           (λ (bind)
             (λ (m* f)
               (λ (dk sk fk)
                 (m*
                  (λ (m^)
                    (dk (bind m^ f)))
                  (λ (b vc c)
                    ((mplus (f b vc) (bind c f))
                     dk
                     sk
                     fk))
                  fk)))))
          (g x)
          h)))))

2. m is a computation that invokes the sk
(λ (dk sk fk)
  (m
   (λ (m^)
     (dk (bind
          m^
          (lambda (x)
            ((fix2
              (λ (bind)
                (λ (m f)
                  (λ (dk sk fk)
                    (m
                     (λ (m^)
                       (dk (bind m^ f)))
                     (λ (b vc c)
                       ((mplus (f b vc) (bind c f))
                        dk
                        sk
                        fk))
                     fk)))))
             (g x)
             h)))))
   (λ (b vc c)
     ((mplus (f b vc)
             (bind
              c
              (lambda (x)
                ((fix2
                  (λ (bind)
                    (λ (m f)
                      (λ (dk sk fk)
                        (m
                         (λ (m^)
                           (dk (bind m^ f)))
                         (λ (b vc c)
                           ((mplus (f b vc) (bind c f))
                            dk
                            sk
                            fk))
                         fk)))))
                 (g x)
                 h))))
      dk
      sk
      fk))
   fk))

3. m is a computation that invokes the fk
(λ (dk sk fk)
  (m
   (λ (m^)
     (dk (bind
          m^
          (lambda (x)
            ((fix2
              (λ (bind)
                (λ (m f)
                  (λ (dk sk fk)
                    (m
                     (λ (m^)
                       (dk (bind m^ f)))
                     (λ (b vc c)
                       ((mplus (f b vc) (bind c f))
                        dk
                        sk
                        fk))
                     fk)))))
             (g x)
             h)))))
   (λ (b vc c)
     ((mplus (f b vc)
             (bind
              c
              (lambda (x)
                ((fix2
                  (λ (bind)
                    (λ (m f)
                      (λ (dk sk fk)
                        (m
                         (λ (m^)
                           (dk (bind m^ f)))
                         (λ (b vc c)
                           ((mplus (f b vc) (bind c f))
                            dk
                            sk
                            fk))
                         fk)))))
                 (g x)
                 h))))
      dk
      sk
      fk))
   fk))
