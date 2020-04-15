#lang racket

(struct later (value) #:transparent)
(struct now (value) #:transparent)

(define (ee g)
  (match g
    [`(d ,g1 ,g2) 
     ($append (ee g1)
              (later 
               (λ () 
                 (ee g2))))]
    [`(c ,g1 ,g2) ($append-map (ee g1) g2)]
    [`(s) (now (cons '#t (now '())))]
    [`(f) (now '())]
    [`(appo) 
     (later 
      (λ () 
        (ee `(d (c (s) (s)) (c (s) (appo))))))]
    [`(unpo)
     (later
      (λ () 
        (ee '(unpo))))]
    [`(listo)
     (later
      (λ () 
        (ee '(d (c (s) (s)) (listo)))))]))

(define ($append $1 $2)
  (match $1
    [(now str)
     (match str
       [`() $2]
       [`(,s . ,$3) (now (cons s ($append $3 $2)))])]
    [(later $k) 
     (later 
      (λ () 
        ($append ($k) $2)))]))

(define ($append-map $1 g)
  (match $1
    [(now str)
     (match str
       [`() (now '())]
       [`(,s . ,$3) 
        ($append (ee g) 
                 (later 
                  (λ ()
                    ($append-map $3 g))))])]
    [(later $k)
     (later 
      (λ ()
        ($append-map ($k) g)))]))
