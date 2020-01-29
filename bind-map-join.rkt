#lang racket

;; We know bind relates to map and join

;; (bind m f) = (join (map f m))
;; bind = join o map

;; So, given an implementation of bind, we should be able to decompose
;; it to a map and a join.

;; What does an f look like?
;; a -> b
;; add1, e.g.

;; These definitions assume end, later, and cons constructors.

(define ($map $1 f)
  (match $1
    [(end) $1]
    [(later $k) (later (λ () ($map ($k) f)))] ;; this looks like invoking an fk. 
    [(cons u $3) (cons (f u) ($map f $3))]))

;; $join :: MMα → Mα 
(define ($join $$1)
  (match $$1
    [(end) $$1]
    [(later $k) $$1]
    [(cons $1 $$3) ($append $1 ($join $$3))]))

;; whereas for bind an f looks like a -> M b

;; Notice, though that this implementation depends on the $append
;; function.
