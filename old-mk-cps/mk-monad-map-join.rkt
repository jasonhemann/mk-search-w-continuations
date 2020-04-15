#lang racket

(struct later (value) #:transparent)
(struct end   () #:transparent)
(struct cons  (a d) #:transparent)
(struct 𝕀     ()) ;; fake a unit constructor. Not unit of the monad, real unit. 

;; JStream a = End | More a (JStream a) | Later of (𝕀 → JStream a)

;; Church Encode the datatype
;; (define end
;;   (λ (sm)
;;     (λ (sl)
;;       (λ (se) ;; because it's an fk, and that's the way they do it in Danvy
;;         (se)))))

;; (define (more x xs)
;;   (λ (sm)
;;     (λ (sl)
;;       (λ (se)
;;         (sm x xs)))))

;; (define (later th) ;; questionable.
;;   (λ (sm)
;;     (λ (sl)
;;       (λ (se)
;;         sl))))


;; f is (λ (unit) (ee g))
;; This wants to be curried out as
;; such for our programming situation

;; unit :: α → Mα 
(define (unit a) (cons a (end)))

;; $map :: (α → β) → Mα → Mβ
(define ($map f $1)
  (match $1
    [(end) $1]
    [(later $k) (later (λ () ($map f ($k))))] ;; this looks like invoking an fk. 
    [(cons u $3) (cons (f u) ($map f $3))]))

;; $join :: MMα → Mα 
(define ($join $$1)
  (match $$1
    [(end) $$1]
    [(later $k) $$1]
    [(cons $1 $$3) ($append $1 ($join $$3))]))

;; $append :: Mα → Mα → Mα 
(define ($append $1 $2)
  (match $1
    [(end) $2]
    [(later $k) (later (λ () ($append ($k) $2)))] ;; The interleave or not. 
    [(cons u $3) (cons u ($append $3 $2))]))

(define (bind f ma) ($join ($map f ma)))
(define ($append-map g $) (bind (ee g) $))

(define (bind ma f)
  (lambda (sk)
    (lambda (fk)
      ((ma
        (lambda (s)
          (lambda (fk) 
            (if (ma? s)
                ;; if we have a proc,
                ;; we return a proc 
                ;; such that when we continue
                ;; we try remaining work of bind
                ((sk (lambda (sk)
                       (lambda (fk)
                         (((bind s f) sk) 
                          fk))))
                 fk)
                (((f s)
                  sk)
                 fk)))))
       fk))))

(define (disj $1 $2) ;; should this recursively call disj? 
  (lambda (sk)
    (lambda (fk)
      (($1 (lambda (s)
             (lambda (fk)
               (if (ma? s)
                   ((sk (lambda (sk)
                          (lambda (fk)
                            (disj $2 )))))
                   ((sk s)
                    fk)))))
       (lambda ()
         (($2 sk)
          fk))))))

                   ;; ((s (lambda (sk)
                   ;;       (lambda (fk)
                   ;;         (($2 sk)
                   ;;          fk))))
                   ;;  fk)

(define (allo)
  (lambda (sk)
    (lambda (fk)
      ((sk (lambda (sk)
             (lambda (fk)
               (ee '(d s (allo)) sk fk))))
       fk))))

(define ((((ee g) st) sk) fk)
  (match g
    [`s ((sk st) fk)]
    [`f (fk)]
    [`(cong ,g1 ,g2) 
     (bind ((ee g2) st) ((((ee g1) st) sk) fk))]
    [`(disj ,g1 ,g2) 
;;     ($append ((ee g1) s) ((ee g2) s))
     ]
    [`(allo)
     ((sk (λ ()
            (λ (sk)
              (λ (fk)
                (valof '(d s (allo))
                       s
                       sk
                       fk)))))
      fk)]))


;; (define (η a)
;;   (λ (κˢ) 
;;     (λ (κᶠ)
;;       ((κˢ a) κᶠ))))

;; (define (map f xs)
;;   (λ (κˢ)
;;     (λ (κᶠ)
;;       ((xs
;;         (λ (x)
;;           (κˢ 
;;            (f x))))
;;        κᶠ))))

;; (define (join ls)
;;   (λ (κˢ)
;;     (λ (κᶠ)
;;       ((ls
;;         (λ (x) 
;;           (x κˢ)))
;;        κᶠ))))


;; Tail recursive implementation
;; 

;; ;; unit :: α → Mα 
;; (define (η x) =
;;   (λ (κ)
;;     (κ x)))

;; ;; $map :: (α → β) → Mα → Mβ
;; (define ($map f xs) 
;;   (λ (κ) 
;;     (xs
;;      (λ (x) 
;;        (κ (f x))))))

;; ;; $join :: MMα → Mα 
;; (define ($join $$1)
;;   (λ (κ)
;;     ($$1 
;;      (λ (x) 
;;        (x κ)))))




