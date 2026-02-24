#lang racket

;; Something I'm not understanding about the CPS hierarchy and


(define (eval e k1 k2)
  (match e
    [`,x #:when (symbol? x) (pretty-print x) (k1 x k2)]
;;  [`(fail) (k1 '_ k2)] ;; Different undesirable semantics
    [`(fail) (k2 '_)]
    [`(amb ,a ,b) (eval a k1 (lambda (_) (eval b k1 k2)))]))

(define (init-k1)
  (lambda (v k2)
    (k2 v)))

(define (init-k2)
  (lambda (v)
    v))

