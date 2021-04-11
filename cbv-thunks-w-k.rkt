#lang racket


(define (valof-cps e env-cps k)
  (match e
    [`,c #:when (or (number? c) (boolean? c))
         (k c)]
    [`,y #:when (symbol? y)
         (env-cps y k)]
    [`(delay ,e1) (lambda (k) (valof-cps env-cps k))]
    [`(force ,d-exp) (valof-cps d-exp env-cps (lambda (del) (del k)))]
    [`(* ,e1 ,e2)
     (valof-cps e1 env-cps
                (lambda (v)
                  (valof-cps e2 env-cps
                             (lambda (w)
                               (k (* v w))))))]
    [`(sub1 ,x)
     (valof-cps x env-cps
                (lambda (v)
                  (k (sub1 v))))]
    [`(zero? ,x)
     (valof-cps x env-cps
                (lambda (v)
                  (k (zero? v))))]
    [`(if ,t ,c ,a)
     (valof-cps t env-cps
                (lambda (b)
                  (if b
                      (valof-cps c env-cps k)
                      (valof-cps a env-cps k))))]
    [`(delay ,e1) (lambda (k) (valof-cps env-cps k))]
    [`(force ,d-exp) (valof-cps d-exp env-cps (lambda (del) (del k)))]
    [`(lambda (,x) ,body)
     (lambda (a k)
       (valof-cps body
                  (lambda (y k^)
                    (if (eqv? x y)
                        (k^ a)
                        (env-cps y k^)))
                  k))] 
    [`(,rator ,rand)
     (valof-cps rator env-cps
                (lambda (p-cps)
                  (valof-cps rand env-cps
                             (lambda (a)
                               (p-cps a k)))))]
    ;; [(letcc ,k-id ,body) (valof-cps body (lambda (y kbar) (if (eq? k-id y) 
    ;;                                                                                    (kbar k) 
    ;;                                                                                    (env-cps y kbar))) k)]
    ;; [(throw ,rand ,k-exp) (valof-cps k-exp env-cps (lambda (kbar) (valof-cps rand env-cps (lambda (kbar a)))))]

    ))


(define (empty-k)
  (lambda (v)
    v))
