#lang racket
(require rackunit)
;; Dropped dk from sk
;; i.e sk :: a -> fk -> ans

(define unit
  (λ (a)
    (λ (dk)
      (λ (sk) (λ (fk) ((sk a) fk))))))

(define fail
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (fk)))))

(define ((disj c1) c2)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (((c1 (λ (c1^) (dk ((disj c2) c1^)))) sk)
         (λ () (((c2 dk) sk) fk)))))))

(define bind
  (λ (m)
    (λ (f)
      (λ (dk)
        (λ (sk)
          ((m 
            (λ (m^)
              (dk ((bind m^) f))))
           (λ (b)
             (((f b) dk) sk))))))))

(define (map f m^)
  ((bind m^) (λ (a) (unit (f a)))))

(define (join mm^)
  ((bind mm^) identity))

(define kons (λ (a) (λ (fk) (cons a (fk)))))
(define nill (λ () '()))
(define identity (λ (c) c))

(define (looper n)
  (if (zero? n)
      (λ (x) (((x identity) kons) nill))
      (λ (x) (((x (looper (sub1 n))) kons) nill))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (a n)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (dk (b n))))))

(define (b n)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (dk (c n))))))

(define (c n)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (dk ((disj (unit n))
                  (a (add1 n))))))))

(define (d n)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (dk (e n))))))

(define (e n)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (dk ((disj (unit n))
                  (d (add1 n))))))))


(define (pos-3-at-once n)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (dk ((disj (unit n)) ((disj (unit (add1 n))) ((disj (unit (+ 2 n))) (pos-3-at-once (+ 3 n))))))))))

(define (neg-3-at-once n)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (dk ((disj (unit n)) ((disj (unit (sub1 n))) ((disj (unit (- n 2))) (neg-3-at-once (- n 3))))))))))

(define (onenats n)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (dk ((disj (unit n)) (unit (add1 n))))))))

(define (twonats n)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (dk ((disj ((disj (unit n)) (unit (add1 n)))) (unit (add1 (add1 n)))))))))

(define (twonatsrec n)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (dk ((disj ((disj (unit n)) (unit (add1 n)))) (twonatsrec (add1 (add1 n)))))))))

(define (nats n)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (dk ((disj (unit n)) (nats (add1 n))))))))

(define (delay-unit n)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (dk (unit n))))))

(define (nonats n)
  (λ (dk)
    (λ (sk)
      (λ (fk)
        (dk (nonats (add1 n)))))))

;; (disj fail (onenats 4))
;; (map delay-unit (unit 5))
;; (map nats (unit 5))
;; (map unit (onenats 5))
;; (map onenats (unit 5))
;; ((((join (unit (unit 4))) kons) identity) nill)
;; (((((((join (unit (onenats 5))) kons) identity) nill) kons) identity) nill)
;; ((((join (λ (sk) (λ (dk) (λ (fk) (dk (unit 5)))))) kons) identity) nill) kons
;; (((((((join (λ (sk) (λ (dk) (λ (fk) (dk (unit 5)))))) kons) identity) nill) kons) identity) nill)
;; (disj (join (delay-unit (unit 5))) (join (delay-unit (unit 6))))
;; (disj (join (delay-unit (unit 5))) (unit 6))
;; ((( kons) identity) nill)

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


((((join (unit (unit 4))) identity) kons) nill)
(((((((join (unit (onenats 5))) identity) kons) nill) identity) kons) nill)

((((unit 5) identity) kons) nill)

(((((bind (unit 5)) unit) identity) kons) nill)
(((((bind ((bind (unit 5)) unit)) unit) identity) kons) nill)
(((((bind (unit 5)) (λ (x) ((bind (unit x)) unit))) identity) kons) nill)

((((join (map unit (unit 5))) identity) kons) nill)
((((join (map unit (join (map unit (unit 5))))) identity) kons) nill)
((((join (map (λ (x) (join (map unit (unit x)))) (unit 5))) identity) kons) nill)

(((((bind fail) unit) identity) kons) nill)
(((((bind ((bind fail) unit)) unit) identity) kons) nill)
(((((bind fail) (λ (x) ((bind (unit x)) unit))) identity) kons) nill)

((((join (map unit fail)) identity) kons) nill)
((((join (map unit (join (map unit fail)))) identity) kons) nill)
((((join (map (λ (x) (join (map unit (unit x)))) fail)) identity) kons) nill)

(((((bind ((disj (unit 5)) fail)) unit) identity) kons) nill)
(((((bind ((bind ((disj (unit 5)) fail)) unit)) unit) identity) kons) nill)
(((((bind ((disj (unit 5)) fail)) (λ (x) ((bind (unit x)) unit))) identity) kons) nill)

((((join (map unit ((disj (unit 5)) fail))) identity) kons) nill)
((((join (map unit (join (map unit ((disj (unit 5)) fail))))) identity) kons) nill)
((((join (map (λ (x) (join (map unit (unit x)))) ((disj (unit 5)) fail))) identity) kons) nill)

(((((bind ((disj fail) (unit 5))) unit) identity) kons) nill)
(((((bind ((bind ((disj fail) (unit 5))) unit)) unit) identity) kons) nill)
(((((bind ((disj fail) (unit 5))) (λ (x) ((bind (unit x)) unit))) identity) kons) nill)

((((join (map unit ((disj fail) (unit 5)))) identity) kons) nill)
((((join (map unit (join (map unit ((disj fail) (unit 5)))))) identity) kons) nill)
((((join (map (λ (x) (join (map unit (unit x)))) ((disj fail) (unit 5)))) identity) kons) nill)

(((((bind fail) (λ (x) ((disj (unit x)) (unit x)))) identity) kons) nill)
(((((bind ((bind fail) (λ (x) ((disj (unit x)) (unit x))))) (λ (x) ((disj (unit x)) (unit x)))) identity) kons) nill)
(((((bind fail) (λ (x) ((bind (unit x)) (λ (x) ((disj (unit x)) (unit x)))))) identity) kons) nill)

((((join (map (λ (x) ((disj (unit x)) (unit x))) fail)) identity) kons) nill)
((((join (map (λ (x) ((disj (unit x)) (unit x))) (join (map (λ (x) ((disj (unit x)) (unit x))) fail)))) identity) kons) nill)
((((join (map (λ (x) (join (map (λ (x) ((disj (unit x)) (unit x))) (unit x)))) fail)) identity) kons) nill)

(((((bind (unit 5)) (λ (x) ((disj (unit x)) (unit x)))) identity) kons) nill)
(((((bind ((bind (unit 5)) (λ (x) ((disj (unit x)) (unit x))))) (λ (x) ((disj (unit x)) (unit x)))) identity) kons) nill)
(((((bind (unit 5)) (λ (x) ((bind (unit x)) (λ (x) ((disj (unit x)) (unit x)))))) identity) kons) nill)

((((join (map (λ (x) ((disj (unit x)) (unit x))) (unit 5))) identity) kons) nill)
((((join (map (λ (x) ((disj (unit x)) (unit x))) (join (map (λ (x) ((disj (unit x)) (unit x))) (unit 5))))) identity) kons) nill)
((((join (map (λ (x) (join (map (λ (x) ((disj (unit x)) (unit x))) (unit x)))) (unit 5))) identity) kons) nill)
(((((((delay-unit 7) identity) kons) nill) identity) kons) nill)
((((((((disj fail) (onenats 4)) identity) kons) nill) identity) kons) nill)
(((((((join (delay-unit (unit 5))) identity) kons) nill) identity) kons) nill)
((((join (unit (unit 5))) identity) kons) nill)
(((((((join (unit (delay-unit 5))) identity) kons) nill) identity) kons) nill)
(((((((join (unit (onenats 5))) identity) kons) nill) identity) kons) nill)
(define pr (((((((join (unit (nats 5))) identity) kons) nill) identity) kons) nill))
;; (((((((join (map nats (unit 5))) identity) kons) nill) identity) kons) nill)
;; ((((join (unit (nats 5))) identity) kons) nill)

((looper 30) (join (map nats (nats 5))))
((looper 30) ((disj (nats 5)) (nats 100)))
((looper 30) ((disj (nats 5)) ((disj (nats 100)) (nats 200))))
((looper 30) ((disj (nonats 5)) ((disj (nats 100)) (nats 200))))
((looper 50) ((disj (twonatsrec 100)) (nats 5)))
((looper 300) (join (map nats (nats 5))))
((looper 10) ((disj (unit 5)) (nonats 6)))
((looper 10) ((disj (unit 5)) ((disj (nonats 6)) (unit 7))))
((looper 10) ((disj (nats 5)) ((disj (nonats 6)) (nats 7))))
((looper 10) ((disj (nonats 6)) (unit 5)))

((((((((((join (map nats (nats 5))) identity) kons) nill) identity)
     kons) nill) identity) kons) nill)






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



;; (define bind
;;   (λ (m)
;;     (λ (f)
;;       (λ (sk)
;;         (λ (fk)
;;           (λ (dk)
;;             (((m
;;                (λ (b)
;;                  (λ (fk)
;;                    ((((f b) sk) fk) 
;;                     (λ (mm^)
;;                       (dk (join mm^)))))))
;;               fk)
;;              (λ (m^)
;;                (dk (map f m^))))))))))


;; (define bind
;;   (λ (m)
;;     (λ (f)
;;       (λ (sk)
;;         (λ (fk)
;;           (λ (dk)
;;             (((m (λ (b)
;;                    (λ (fk)
;;                      ((((f b) sk)
;;                        fk)
;;                       dk))))
;;               fk)
;;              (λ (m^)
;;                (dk ((bind m^) f))))))))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (a n)
;;   (λ (sk)
;;     (λ (dk)
;;       (λ (fk)
;;         (dk (b n))))))

;; (define (b n)
;;   (λ (sk)
;;     (λ (dk)
;;       (λ (fk)
;;         (dk (c n))))))

;; (define (c n)
;;   (λ (sk)
;;     (λ (dk)
;;       (λ (fk)
;;         (dk (disj (unit n)
;;                   (a (add1 n))))))))

;; (define (d n)
;;   (λ (sk)
;;     (λ (dk)
;;       (λ (fk)
;;         (dk (e n))))))

;; (define (e n)
;;   (λ (sk)
;;     (λ (dk)
;;       (λ (fk)
;;         (dk (disj (unit n)
;;                   (d (add1 n))))))))


;; (define (pos-3-at-once n)
;;   (λ (sk)
;;     (λ (dk)
;;       (λ (fk)
;;         (dk (disj (unit n) (disj (unit (add1 n)) (disj (unit (+ 2 n)) (pos-3-at-once (+ 3 n))))))))))

;; (define (neg-3-at-once n)
;;   (λ (sk)
;;     (λ (dk)
;;       (λ (fk)
;;         (dk (disj (unit n) (disj (unit (sub1 n)) (disj (unit (- n 2)) (neg-3-at-once (- n 3))))))))))

;; (define (onenats n)
;;   (λ (sk)
;;     (λ (dk)
;;       (λ (fk)
;;         (dk (disj (unit n) (unit (add1 n))))))))

;; (define (twonats n)
;;   (λ (sk)
;;     (λ (dk)
;;       (λ (fk)
;;         (dk (disj (disj (unit n) (unit (add1 n))) (unit (add1 (add1 n)))))))))

;; (define (twonatsrec n)
;;   (λ (sk)
;;     (λ (dk)
;;       (λ (fk)
;;         (dk (disj (disj (unit n) (unit (add1 n))) (twonatsrec (add1 (add1 n)))))))))

;; (define (nats n)
;;   (λ (sk)
;;     (λ (dk)
;;       (λ (fk)
;;         (dk (disj (unit n) (nats (add1 n))))))))

;; (define (delay-unit n)
;;   (λ (sk)
;;     (λ (dk)
;;       (λ (fk)
;;         (dk (unit n))))))

;; (define (nonats n)
;;   (λ (sk)
;;     (λ (dk)
;;       (λ (fk)
;;         (dk (nonats (add1 n)))))))

;; ;; (disj fail (onenats 4))
;; ;; (map delay-unit (unit 5))
;; ;; (map nats (unit 5))
;; ;; (map unit (onenats 5))
;; ;; (map onenats (unit 5))
;; ;; ((((join (unit (unit 4))) kons) identity) nill)
;; ;; (((((((join (unit (onenats 5))) kons) identity) nill) kons) identity) nill)
;; ;; ((((join (λ (sk) (λ (dk) (λ (fk) (dk (unit 5)))))) kons) identity) nill) kons
;; ;; (((((((join (λ (sk) (λ (dk) (λ (fk) (dk (unit 5)))))) kons) identity) nill) kons) identity) nill)
;; ;; (disj (join (delay-unit (unit 5))) (join (delay-unit (unit 6))))
;; ;; (disj (join (delay-unit (unit 5))) (unit 6))
;; ;; ((( kons) identity) nill)

;; ;; Bind in terms of bind, thought about some more 
;; ;; (define bind 
;; ;;   (λ (m)
;; ;;     (λ (f)
;; ;;       (λ (sk)
;; ;;         (λ (dk)
;; ;;           (λ (fk)
;; ;;             (((m
;; ;;                (λ (b)
;; ;;                  (λ (dk^)
;; ;;                    (λ (fk)
;; ;;                      ((((f b) sk)
;; ;;                        dk)
;; ;;                       fk)))))
;; ;;               (λ (m^)
;; ;;                 (dk ((bind m^) f))))
;; ;;              fk)))))))

;; ;; (define (map f m)
;; ;;   ((bind m) (λ (a) (unit (f a)))))

;; ;; (define bind 
;; ;;   (λ (m)
;; ;;     (λ (f)
;; ;;       (λ (sk)
;; ;;         (λ (fk)
;; ;;           ((m
;; ;;             (λ (b)
;; ;;               (λ (fk)
;; ;;                 (((f b)
;; ;;                   sk)
;; ;;                  fk))))
;; ;;            fk))))))

;; ;; (define (join z)
;; ;;   ((bind z) (λ (m) m)))

;; ;; (define (map f m)
;; ;;   (bind m (λ (a) (unit (f a)))))

;; ;; (define (bind m f)
;; ;;   (λ (sk)
;; ;;     (λ (fk)
;; ;;       ((m
;; ;;         (λ (b)
;; ;;           (λ (fk)
;; ;;             (((f b)
;; ;;               sk)
;; ;;              fk))))
;; ;;        fk))))

;; ;; (define (join z)
;; ;;   (bind z (λ (m) m)))

;; ;; (define (bind m f)
;; ;;   (λ (sk)
;; ;;     (λ (fk)
;; ;;       ((m
;; ;;         (λ (b)
;; ;;           (λ (fk)
;; ;;             (((f b)
;; ;;               sk)
;; ;;              fk))))
;; ;;        fk))))

;; ;; (define (bind m f)
;; ;;   (λ (sk)
;; ;;     (λ (fk)
;; ;;       ((m
;; ;;         (λ (b)
;; ;;           (λ (fk)
;; ;;             (((f b)
;; ;;               sk)
;; ;;              fk))))
;; ;;        fk))))


;; ((((join (unit (unit 4))) kons) identity) nill)
;; (((((((join (unit (onenats 5))) kons) identity) nill) kons) identity) nill)

;; ((((unit 5) kons) identity) nill)

;; (((((bind (unit 5)) unit) kons) identity) nill)
;; (((((bind ((bind (unit 5)) unit)) unit) kons) identity) nill)
;; (((((bind (unit 5)) (λ (x) ((bind (unit x)) unit))) kons) identity) nill)

;; ((((join (map unit (unit 5))) kons) identity) nill)
;; ((((join (map unit (join (map unit (unit 5))))) kons) identity) nill)
;; ((((join (map (λ (x) (join (map unit (unit x)))) (unit 5))) kons) identity) nill)

;; (((((bind fail) unit) kons) identity) nill)
;; (((((bind ((bind fail) unit)) unit) kons) identity) nill)
;; (((((bind fail) (λ (x) ((bind (unit x)) unit))) kons) identity) nill)

;; ((((join (map unit fail)) kons) identity) nill)
;; ((((join (map unit (join (map unit fail)))) kons) identity) nill)
;; ((((join (map (λ (x) (join (map unit (unit x)))) fail)) kons) identity) nill)

;; (((((bind (disj (unit 5) fail)) unit) kons) identity) nill)
;; (((((bind ((bind (disj (unit 5) fail)) unit)) unit) kons) identity) nill)
;; (((((bind (disj (unit 5) fail)) (λ (x) ((bind (unit x)) unit))) kons) identity) nill)

;; ((((join (map unit (disj (unit 5) fail))) kons) identity) nill)
;; ((((join (map unit (join (map unit (disj (unit 5) fail))))) kons) identity) nill)
;; ((((join (map (λ (x) (join (map unit (unit x)))) (disj (unit 5) fail))) kons) identity) nill)

;; (((((bind (disj fail (unit 5))) unit) kons) identity) nill)
;; (((((bind ((bind (disj fail (unit 5))) unit)) unit) kons) identity) nill)
;; (((((bind (disj fail (unit 5))) (λ (x) ((bind (unit x)) unit))) kons) identity) nill)

;; ((((join (map unit (disj fail (unit 5)))) kons) identity) nill)
;; ((((join (map unit (join (map unit (disj fail (unit 5)))))) kons) identity) nill)
;; ((((join (map (λ (x) (join (map unit (unit x)))) (disj fail (unit 5)))) kons) identity) nill)

;; (((((bind fail) (λ (x) (disj (unit x) (unit x)))) kons) identity) nill)
;; (((((bind ((bind fail) (λ (x) (disj (unit x) (unit x))))) (λ (x) (disj (unit x) (unit x)))) kons) identity) nill)
;; (((((bind fail) (λ (x) ((bind (unit x)) (λ (x) (disj (unit x) (unit x)))))) kons) identity) nill)

;; ((((join (map (λ (x) (disj (unit x) (unit x))) fail)) kons) identity) nill)
;; ((((join (map (λ (x) (disj (unit x) (unit x))) (join (map (λ (x) (disj (unit x) (unit x))) fail)))) kons) identity) nill)
;; ((((join (map (λ (x) (join (map (λ (x) (disj (unit x) (unit x))) (unit x)))) fail)) kons) identity) nill)

;; (((((bind (unit 5)) (λ (x) (disj (unit x) (unit x)))) kons) identity) nill)
;; (((((bind ((bind (unit 5)) (λ (x) (disj (unit x) (unit x))))) (λ (x) (disj (unit x) (unit x)))) kons) identity) nill)
;; (((((bind (unit 5)) (λ (x) ((bind (unit x)) (λ (x) (disj (unit x) (unit x)))))) kons) identity) nill)

;; ((((join (map (λ (x) (disj (unit x) (unit x))) (unit 5))) kons) identity) nill)
;; ((((join (map (λ (x) (disj (unit x) (unit x))) (join (map (λ (x) (disj (unit x) (unit x))) (unit 5))))) kons) identity) nill)
;; ((((join (map (λ (x) (join (map (λ (x) (disj (unit x) (unit x))) (unit x)))) (unit 5))) kons) identity) nill)
;; (((((((delay-unit 7) kons) identity) nill) kons) identity) nill)
;; (((((((disj fail (onenats 4)) kons) identity) nill) kons) identity) nill)
;; (((((((join (delay-unit (unit 5))) kons) identity) nill) kons) identity) nill)
;; ((((join (unit (unit 5))) kons) identity) nill)
;; (((((((join (unit (delay-unit 5))) kons) identity) nill) kons) identity) nill)
;; (((((((join (unit (onenats 5))) kons) identity) nill) kons) identity) nill)
;; (define pr (((((((join (unit (nats 5))) kons) identity) nill) kons) identity) nill))
;; ;; (((((((join (map nats (unit 5))) kons) identity) nill) kons) identity) nill)
;; ;; ((((join (unit (nats 5))) kons) identity) nill)

;; ((looper 30) (join (map nats (nats 5))))
;; ((looper 30) (disj (nats 5) (nats 100)))
;; ((looper 30) (disj (nats 5) (disj (nats 100) (nats 200))))
;; ((looper 30) (disj (nonats 5) (disj (nats 100) (nats 200))))
;; ((looper 50) (disj (twonatsrec 100) (nats 5)))
;; ((looper 300) (join (map nats (nats 5))))
;; ((looper 10) (disj (unit 5) (nonats 6)))
;; ((looper 10) (disj (unit 5) (disj (nonats 6) (unit 7))))
;; ((looper 10) (disj (nats 5) (disj (nonats 6) (nats 7))))
;; ((looper 10) (disj (nonats 6) (unit 5)))
;; ((looper 300) (disj (a 20) (d 5)))
;; ((((((((((join (map nats (nats 5))) kons) identity) nill) kons)
;;      identity) nill) kons) identity) nill)






;; ;; should sk take a dk? (e.g. sk :: a -> (ma -> ma) -> (𝕀 -> [a])??
;; ;;   if sk takes a dk, what dk should map use 
;; ;;   if sk takes a dk, what dk should join use
;; ;; should fk take a c?
;; ;; if fk takes a c, what c should we give to actually *invoke* (run) the fk in fail
;; ;; what places, and how, do we extend the dk?
;; ;; what should the computations returned to a dk in a relation definition look like?
;; ;; what should the computations returned to a dk in disj look like?
;; ;; what should the computations returned to a dk in map look like?
;; ;; what should the computations returned to a dk in join look like?



;; ;; (define bind
;; ;;   (λ (m)
;; ;;     (λ (f)
;; ;;       (λ (sk)
;; ;;         (λ (fk)
;; ;;           (λ (dk)
;; ;;             (((m
;; ;;                (λ (b)
;; ;;                  (λ (fk)
;; ;;                    ((((f b) sk) fk) 
;; ;;                     (λ (mm^)
;; ;;                       (dk (join mm^)))))))
;; ;;               fk)
;; ;;              (λ (m^)
;; ;;                (dk (map f m^))))))))))


;; ;; (define bind
;; ;;   (λ (m)
;; ;;     (λ (f)
;; ;;       (λ (sk)
;; ;;         (λ (fk)
;; ;;           (λ (dk)
;; ;;             (((m (λ (b)
;; ;;                    (λ (fk)
;; ;;                      ((((f b) sk)
;; ;;                        fk)
;; ;;                       dk))))
;; ;;               fk)
;; ;;              (λ (m^)
;; ;;                (dk ((bind m^) f))))))))))