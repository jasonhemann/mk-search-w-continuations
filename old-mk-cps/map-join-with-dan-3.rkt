#lang racket
(require rackunit)

;; should sk take a dk? (e.g. sk :: a -> (ma -> ma) -> (𝕀 -> [a])??
;;   if sk takes a dk, what dk should map use 
;;   if sk takes a dk, what dk should join use
;; should fk take a c?
;; if fk takes a c, what c should we give to actually *invoke* (run) the fk in fail
;; what places, and how, do we extend the dk?
;; what should the computations returned to a dk in a relation definition look like?
;; what should the computations returned to a dk in disj look like?
;; what should the computations returned to a dk in map look like?
;; what should the computations returned to a dk in join look like?

(define (onenats n)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (dk (disj (unit n) (unit (add1 n))))))))

(define (nats n)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (dk (disj (unit n) (nats (add1 n))))))))

(define (nonats n)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (dk (nonats (add1 n)))))))

(define unit
  (λ (a)
    (λ (sk)
      (λ (dk)
        (λ (fk)
          (((sk a) dk) fk))))))

(define fail
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (fk)))))

;; (disj fail (onenats 4))
(define (disj c1 c2)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (((c1 sk)
          (λ (c1^)
            (dk (disj c2 c1^))))
         (λ ()
           (((c2 sk)
             dk) ;; this is right b/c (\c.(dk (disj c fail))) =disj+η dk
            fk)))))))

(define (delay-unit n)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (dk (unit n))))))

;; (map delay-unit (unit 5))
;; (map nats (unit 5))
;; (map unit (onenats 5))
;; (map onenats (unit 5))
(define (map f m)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (((m
           (λ (b)
             (λ (dk^) 
               (λ (fk)
                 (((sk (f b)) dk) fk)))))
          (λ (m^)
            (dk (map f m^))))
         fk)))))

;; ((((join (unit (unit 4))) kons) identity) nill)
;; (((((((join (unit (onenats 5))) kons) identity) nill) kons) identity) nill)
;; ((((join (λ (sk) (λ (dk) (λ (fk) (dk (unit 5)))))) kons) identity) nill) kons
;; (((((((join (λ (sk) (λ (dk) (λ (fk) (dk (unit 5)))))) kons) identity) nill) kons) identity) nill)
;; (disj (join (delay-unit (unit 5))) (join (delay-unit (unit 6))))
;; (disj (join (delay-unit (unit 5))) (unit 6))
;; ((( kons) identity) nill)
(define (join mm)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (((mm
           (λ (mb)
             (λ (dk^)
               (λ (fk) 
                 (((mb sk)
                   dk)
                  fk)))))
          (λ (mm^)
            (dk (join mm^))))
         fk)))))

(define bind
  (λ (m)
    (λ (f)
      (join (map f m)))))

;; Bind in terms of bind, thought about some more 
;; (define bind 
;;   (λ (m)
;;     (λ (f)
;;       (λ (sk)
;;         (λ (dk)
;;           (λ (fk)
;;             (((m
;;                (λ (b)
;;                  (λ (dk^)
;;                    (λ (fk)
;;                      ((((f b) sk)
;;                        dk)
;;                       fk)))))
;;               (λ (m^)
;;                 (dk ((bind m^) f))))
;;              fk)))))))

;; (define (map f m)
;;   ((bind m) (λ (a) (unit (f a)))))

;; (define bind 
;;   (λ (m)
;;     (λ (f)
;;       (λ (sk)
;;         (λ (fk)
;;           ((m
;;             (λ (b)
;;               (λ (fk)
;;                 (((f b)
;;                   sk)
;;                  fk))))
;;            fk))))))

;; (define (join z)
;;   ((bind z) (λ (m) m)))

;; (define (map f m)
;;   (bind m (λ (a) (unit (f a)))))

;; (define (bind m f)
;;   (λ (sk)
;;     (λ (fk)
;;       ((m
;;         (λ (b)
;;           (λ (fk)
;;             (((f b)
;;               sk)
;;              fk))))
;;        fk))))

;; (define (join z)
;;   (bind z (λ (m) m)))

;; (define (bind m f)
;;   (λ (sk)
;;     (λ (fk)
;;       ((m
;;         (λ (b)
;;           (λ (fk)
;;             (((f b)
;;               sk)
;;              fk))))
;;        fk))))

;; (define (bind m f)
;;   (λ (sk)
;;     (λ (fk)
;;       ((m
;;         (λ (b)
;;           (λ (fk)
;;             (((f b)
;;               sk)
;;              fk))))
;;        fk))))

(define kons (λ (a) (λ (dk) (λ (fk) (cons a (fk))))))
(define nill (λ () '()))
(define identity (λ (c) c))



((((join (unit (unit 4))) kons) identity) nill)
(((((((join (unit (onenats 5))) kons) identity) nill) kons) identity) nill)

((((unit 5) kons) identity) nill)

(((((bind (unit 5)) unit) kons) identity) nill)
(((((bind ((bind (unit 5)) unit)) unit) kons) identity) nill)
(((((bind (unit 5)) (λ (x) ((bind (unit x)) unit))) kons) identity) nill)

((((join (map unit (unit 5))) kons) identity) nill)
((((join (map unit (join (map unit (unit 5))))) kons) identity) nill)
((((join (map (λ (x) (join (map unit (unit x)))) (unit 5))) kons) identity) nill)

(((((bind fail) unit) kons) identity) nill)
(((((bind ((bind fail) unit)) unit) kons) identity) nill)
(((((bind fail) (λ (x) ((bind (unit x)) unit))) kons) identity) nill)

((((join (map unit fail)) kons) identity) nill)
((((join (map unit (join (map unit fail)))) kons) identity) nill)
((((join (map (λ (x) (join (map unit (unit x)))) fail)) kons) identity) nill)

(((((bind (disj (unit 5) fail)) unit) kons) identity) nill)
(((((bind ((bind (disj (unit 5) fail)) unit)) unit) kons) identity) nill)
(((((bind (disj (unit 5) fail)) (λ (x) ((bind (unit x)) unit))) kons) identity) nill)

((((join (map unit (disj (unit 5) fail))) kons) identity) nill)
((((join (map unit (join (map unit (disj (unit 5) fail))))) kons) identity) nill)
((((join (map (λ (x) (join (map unit (unit x)))) (disj (unit 5) fail))) kons) identity) nill)

(((((bind (disj fail (unit 5))) unit) kons) identity) nill)
(((((bind ((bind (disj fail (unit 5))) unit)) unit) kons) identity) nill)
(((((bind (disj fail (unit 5))) (λ (x) ((bind (unit x)) unit))) kons) identity) nill)

((((join (map unit (disj fail (unit 5)))) kons) identity) nill)
((((join (map unit (join (map unit (disj fail (unit 5)))))) kons) identity) nill)
((((join (map (λ (x) (join (map unit (unit x)))) (disj fail (unit 5)))) kons) identity) nill)

(((((bind fail) (λ (x) (disj (unit x) (unit x)))) kons) identity) nill)
(((((bind ((bind fail) (λ (x) (disj (unit x) (unit x))))) (λ (x) (disj (unit x) (unit x)))) kons) identity) nill)
(((((bind fail) (λ (x) ((bind (unit x)) (λ (x) (disj (unit x) (unit x)))))) kons) identity) nill)

((((join (map (λ (x) (disj (unit x) (unit x))) fail)) kons) identity) nill)
((((join (map (λ (x) (disj (unit x) (unit x))) (join (map (λ (x) (disj (unit x) (unit x))) fail)))) kons) identity) nill)
((((join (map (λ (x) (join (map (λ (x) (disj (unit x) (unit x))) (unit x)))) fail)) kons) identity) nill)

(((((bind (unit 5)) (λ (x) (disj (unit x) (unit x)))) kons) identity) nill)
(((((bind ((bind (unit 5)) (λ (x) (disj (unit x) (unit x))))) (λ (x) (disj (unit x) (unit x)))) kons) identity) nill)
(((((bind (unit 5)) (λ (x) ((bind (unit x)) (λ (x) (disj (unit x) (unit x)))))) kons) identity) nill)

((((join (map (λ (x) (disj (unit x) (unit x))) (unit 5))) kons) identity) nill)
((((join (map (λ (x) (disj (unit x) (unit x))) (join (map (λ (x) (disj (unit x) (unit x))) (unit 5))))) kons) identity) nill)
((((join (map (λ (x) (join (map (λ (x) (disj (unit x) (unit x))) (unit x)))) (unit 5))) kons) identity) nill)
(((((((delay-unit 7) kons) identity) nill) kons) identity) nill)
(((((((disj fail (onenats 4)) kons) identity) nill) kons) identity) nill)
(((((((join (delay-unit (unit 5))) kons) identity) nill) kons) identity) nill)
((((join (unit (unit 5))) kons) identity) nill)
(((((((join (unit (delay-unit 5))) kons) identity) nill) kons) identity) nill)
(((((((join (unit (onenats 5))) kons) identity) nill) kons) identity) nill)
(define pr (((((((join (unit (nats 5))) kons) identity) nill) kons) identity) nill))
;; (((((((join (map nats (unit 5))) kons) identity) nill) kons) identity) nill)
;; ((((join (unit (nats 5))) kons) identity) nill)
(define (looper n)
  (if (zero? n)
   (λ (x)
      (((x kons) identity) nill))
   (λ (x)
      (((x kons) (looper (sub1 n))) nill))))

((looper 30) (join (map nats (nats 5))))

;; ((((((((((join (map nats (nats 5))) kons) identity) nill) kons) identity) nill) kons) identity) nill)


