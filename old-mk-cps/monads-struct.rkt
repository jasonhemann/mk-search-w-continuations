#lang racket
(provide return-maybe fail bind-maybe run-maybe
         return-writer tell-writer bind-writer run-writer
         return-state bind-state run-state
         return-cont bind-cont callcc run-cont
         do)

(define-syntax do
  (syntax-rules (<-)
    ((_ bind e) e)
    ((_ bind (v <- e) e* e** ...)
     (bind e (lambda (v) (do bind e* e** ...))))
    ((_ bind e e* e** ...)
     (bind e (lambda (_) (do bind e* e** ...))))))

(struct return-maybe (a) #:reflection-name 'Just #:transparent)
(struct fail () #:reflection-name 'Nothing #:transparent)

(define (bind-maybe ma f)
  (match ma
    [(return-maybe v) (f v)]
    [(fail) (fail)]))

(define/match (run-maybe ma)
  [((return-maybe v)) v]
  [((fail)) (error 'run-maybe "attempted run from computation without pure value")])

(struct writer (a lg) #:transparent)
(define (return-writer a) (writer a '()))
(define (tell-writer msg) (writer (void) `(,msg)))
(define (bind-writer ma f)
  (match-let* ([(writer v la) ma]
               [(writer v lb) (f v)])
    (writer v (append la lb))))

(define/match (run-writer ma)
  [((writer (? void?) l)) (error 'run-writer "attempted run from computation without pure value")]
  [((writer a l)) a])

(define (apply-state ma s)
  (match ma
    [(return-state a) (values a s)]
    [(get-state) (values s s)]
    [(put-state s) (values (void) s)]
    [(bind-state ma f) (let-values ([(a s) (apply-state ma s)])
                         (apply-state (f a) s))]))

;; How are these useful information to have? 
(struct return-state (a) #:reflection-name 'return #:transparent)
(struct bind-state (ma f) #:reflection-name 'bind #:transparent)
(struct put-state (s) #:reflection-name 'put #:transparent)
(struct get-state () #:reflection-name 'get #:transparent)

;; values->list not provided in racket
(define (run-state ma s) 
  (match/values (apply-state ma s)
    [((? void?) s) (error 'run-state "attempted run from computation without pure value")]
    [(a s) a]))

(define ((return-cont a) k)
  (k a))
(define ((bind-cont ma f) k)
  (let ([k (λ (a)
             (let ([mb (f a)])
               (mb k)))])
    (ma k)))
(define ((callcc g) k)
  (let ([k-as-proc (λ (a) (λ (k^) (k a)))])
    (let ([ma (g k-as-proc)])
      (ma k))))

(define run-cont '???)
