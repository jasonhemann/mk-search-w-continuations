#lang racket

(define unit
  (λ (a)
    (λ (sk)
      (λ (fk)
        ((sk a) fk)))))

(define (map f m)
  (λ (sk)
    (λ (fk)
      ((m
        (λ (b)
          (λ (fk)
            ((sk (f b)) fk))))
       fk))))

(define (join z)
  (λ (sk)
    (λ (fk)
      ((z
        (λ (mb)
          (λ (fk)
            ((mb sk) fk))))
       fk))))

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

(((unit 5) kons) nill)

((((bind (unit 5)) unit) kons) nill)
((((bind ((bind (unit 5)) unit)) unit) kons) nill)
((((bind (unit 5)) (λ (x) ((bind (unit x)) unit))) kons) nill)

(((join (map unit (unit 5))) kons) nill)
(((join (map unit (join (map unit (unit 5))))) kons) nill)
(((join (map (λ (x) (join (map unit (unit x)))) (unit 5))) kons) nill)
