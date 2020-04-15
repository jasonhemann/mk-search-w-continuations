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

(define unit
  (λ (a)
    (λ (sk)
      (λ (dk)
        (λ (fk)
          ((sk a) fk))))))

(define fail
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (fk)))))

(define (disj c1 c2)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (((c1 sk)
          dk)
         (λ ()
           (((c2 sk)
             dk) 
            fk)))))))

(define (map f m)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (((m
           (λ (b)
             (λ (fk)
               ((sk (f b)) fk))))
          dk)
         fk)))))

(define (join z)
  (λ (sk)
    (λ (dk)
      (λ (fk)
        (((z
           (λ (mb)
             (λ (fk) 
               (((mb sk)
                 dk)
                fk))))
          dk)
         fk)))))

(define bind
  (λ (m)
    (λ (f)
      (join (map f m)))))

;; (define bind 
;;   (λ (m)
;;     (λ (f)
;;       (λ (sk)
;;         (λ (fk)
;;           ((m
;;             (λ (b)
;;               (λ (fk)
;;                 (((f b) sk) fk))))
;;            fk))))))

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

(define kons (λ (a) (λ (b) (cons a (b)))))
(define nill (λ () '()))

((((unit 5) kons) (λ (x) x)) nill)

(((((bind (unit 5)) unit) kons) (λ (x) x)) nill)
(((((bind ((bind (unit 5)) unit)) unit) kons) (λ (x) x)) nill)
(((((bind (unit 5)) (λ (x) ((bind (unit x)) unit))) kons) (λ (x) x)) nill)

((((join (map unit (unit 5))) kons) (λ (x) x)) nill)
((((join (map unit (join (map unit (unit 5))))) kons) (λ (x) x)) nill)
((((join (map (λ (x) (join (map unit (unit x)))) (unit 5))) kons) (λ (x) x)) nill)

(((((bind fail) unit) kons) (λ (x) x)) nill)
(((((bind ((bind fail) unit)) unit) kons) (λ (x) x)) nill)
(((((bind fail) (λ (x) ((bind (unit x)) unit))) kons) (λ (x) x)) nill)

((((join (map unit fail)) kons) (λ (x) x)) nill)
((((join (map unit (join (map unit fail)))) kons) (λ (x) x)) nill)
((((join (map (λ (x) (join (map unit (unit x)))) fail)) kons) (λ (x) x)) nill)

(((((bind (disj (unit 5) fail)) unit) kons) (λ (x) x)) nill)
(((((bind ((bind (disj (unit 5) fail)) unit)) unit) kons) (λ (x) x)) nill)
(((((bind (disj (unit 5) fail)) (λ (x) ((bind (unit x)) unit))) kons) (λ (x) x)) nill)

((((join (map unit (disj (unit 5) fail))) kons) (λ (x) x)) nill)
((((join (map unit (join (map unit (disj (unit 5) fail))))) kons) (λ (x) x)) nill)
((((join (map (λ (x) (join (map unit (unit x)))) (disj (unit 5) fail))) kons) (λ (x) x)) nill)

(((((bind (disj fail (unit 5))) unit) kons) (λ (x) x)) nill)
(((((bind ((bind (disj fail (unit 5))) unit)) unit) kons) (λ (x) x)) nill)
(((((bind (disj fail (unit 5))) (λ (x) ((bind (unit x)) unit))) kons) (λ (x) x)) nill)

((((join (map unit (disj fail (unit 5)))) kons) (λ (x) x)) nill)
((((join (map unit (join (map unit (disj fail (unit 5)))))) kons) (λ (x) x)) nill)
((((join (map (λ (x) (join (map unit (unit x)))) (disj fail (unit 5)))) kons) (λ (x) x)) nill)

(((((bind fail) (λ (x) (disj (unit x) (unit x)))) kons) (λ (x) x)) nill)
(((((bind ((bind fail) (λ (x) (disj (unit x) (unit x))))) (λ (x) (disj (unit x) (unit x)))) kons) (λ (x) x)) nill)
(((((bind fail) (λ (x) ((bind (unit x)) (λ (x) (disj (unit x) (unit x)))))) kons) (λ (x) x)) nill)

((((join (map (λ (x) (disj (unit x) (unit x))) fail)) kons) (λ (x) x)) nill)
((((join (map (λ (x) (disj (unit x) (unit x))) (join (map (λ (x) (disj (unit x) (unit x))) fail)))) kons) (λ (x) x)) nill)
((((join (map (λ (x) (join (map (λ (x) (disj (unit x) (unit x))) (unit x)))) fail)) kons) (λ (x) x)) nill)

(((((bind (unit 5)) (λ (x) (disj (unit x) (unit x)))) kons) (λ (x) x)) nill)
(((((bind ((bind (unit 5)) (λ (x) (disj (unit x) (unit x))))) (λ (x) (disj (unit x) (unit x)))) kons) (λ (x) x)) nill)
(((((bind (unit 5)) (λ (x) ((bind (unit x)) (λ (x) (disj (unit x) (unit x)))))) kons) (λ (x) x)) nill)

((((join (map (λ (x) (disj (unit x) (unit x))) (unit 5))) kons) (λ (x) x)) nill)
((((join (map (λ (x) (disj (unit x) (unit x))) (join (map (λ (x) (disj (unit x) (unit x))) (unit 5))))) kons) (λ (x) x)) nill)
((((join (map (λ (x) (join (map (λ (x) (disj (unit x) (unit x))) (unit x)))) (unit 5))) kons) (λ (x) x)) nill)
