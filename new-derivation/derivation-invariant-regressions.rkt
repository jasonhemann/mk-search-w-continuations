#lang racket

;; Derivation regression gate for miniKanren search structure.
;; Purpose:
;; - catch regressions in cross-derivation invariants (v1..v4)
;; - validate delayed map behavior in stream variants
;; - validate map/join == bind and dk preservation in sk/fk variants
;; - sanity-check selected v4 run behavior used by examples
;; Scope:
;; - focused invariant checks, not exhaustive completeness testing

(require rackunit
         rackunit/text-ui
         racket/promise
         racket/runtime-path)

(define-runtime-path derivation-1 "mk-streams-derivation-1.rkt")
(define-runtime-path derivation-2 "mk-streams-derivation-2.rkt")
(define-runtime-path derivation-3 "mk-streams-derivation-3.rkt")
(define-runtime-path derivation-4 "mk-streams-derivation-4.rkt")

(define (module-namespace path submodule)
  (define module-path `(submod (file ,(path->string path)) ,submodule))
  (dynamic-require module-path #f)
  (module->namespace module-path))

(define (ns-get ns sym)
  (parameterize ([current-namespace ns])
    (namespace-variable-value sym #t)))

(define (ns-eval ns expr)
  (parameterize ([current-namespace ns])
    (eval expr)))

(define (observe-stream stream [fuel 80])
  (cond
    [(<= fuel 0) '(FUEL)]
    [(null? stream) '()]
    [(pair? stream) (cons (car stream) (observe-stream (cdr stream) (sub1 fuel)))]
    [(promise? stream) (observe-stream (force stream) (sub1 fuel))]
    [else (list 'NONSTREAM stream)]))

(define (observe-sk/fk computation [fuel 80])
  (define (kons a) (lambda (fk) (cons a (fk))))
  (define (nill) '())
  (define (loop n c)
    (if (<= n 0)
        '(FUEL)
        (((c (lambda (c^) (loop (sub1 n) c^))) kons) nill)))
  (loop fuel computation))

(define (check-stream-map-delay path submodule)
  (define ns (module-namespace path submodule))
  (define map* (ns-get ns 'map))
  (check-equal? (observe-stream ((map* add1) (delay/name '(1 2 3))))
                '(2 3 4)))

(define (check-sk/fk-mapjoin-bind path submodule)
  (define ns (module-namespace path submodule))
  (define unit (ns-get ns 'unit))
  (define mplus (ns-get ns 'mplus))
  (define map* (ns-get ns 'map))
  (define join (ns-get ns 'join))
  (define bind (ns-get ns 'bind))
  (define f (lambda (n) (unit (* n n))))
  (define mm (mplus (unit 5) (unit 6)))
  (define lhs (observe-sk/fk (join ((map* f) mm))))
  (define rhs (observe-sk/fk ((bind mm) f)))
  (check-equal? lhs '(25 36))
  (check-equal? rhs '(25 36))
  (check-equal? lhs rhs))

(define (check-sk/fk-dk-join path submodule)
  (define ns (module-namespace path submodule))
  (define unit (ns-get ns 'unit))
  (define join (ns-get ns 'join))
  (define mb
    (lambda (dk)
      (lambda (sk)
        (lambda (fk)
          (dk (unit 5))))))
  (define lhs (observe-sk/fk (join (unit mb))))
  (define rhs (observe-sk/fk mb))
  (check-equal? lhs '(5))
  (check-equal? rhs '(5))
  (check-equal? lhs rhs))

(define (check-v4-l/m-run-prefix)
  (define ns (module-namespace derivation-4 'streams-unit-map-join))
  (define run (ns-get ns 'run))
  (ns-eval ns '(define-relation (l-sure n) (m-sure n)))
  (ns-eval ns '(define-relation (m-sure n) (mplus (l-sure n) (unit n))))
  (define l-sure (ns-eval ns 'l-sure))
  (check-equal? (run 7 (l-sure 5)) '(5 5 5)))

(define derivation-invariant-tests
  (test-suite
   "derivation invariant regressions"
   (test-case "v1 streams-unit-map-join delayed map" (check-stream-map-delay derivation-1 'streams-unit-map-join))
   (test-case "v1 streams-bind-return delayed map" (check-stream-map-delay derivation-1 'streams-bind-return))
   (test-case "v2 streams-unit-map-join delayed map" (check-stream-map-delay derivation-2 'streams-unit-map-join))
   (test-case "v3 streams-unit-map-join delayed map" (check-stream-map-delay derivation-3 'streams-unit-map-join))
   (test-case "v4 streams-unit-map-join delayed map" (check-stream-map-delay derivation-4 'streams-unit-map-join))
   (test-case "v2 sk/fk-unit-map-join map/join == bind" (check-sk/fk-mapjoin-bind derivation-2 'sk/fk-unit-map-join))
   (test-case "v2 sk/fk-bind-return map/join == bind" (check-sk/fk-mapjoin-bind derivation-2 'sk/fk-bind-return))
   (test-case "v3 sk/fk-unit-map-join map/join == bind" (check-sk/fk-mapjoin-bind derivation-3 'sk/fk-unit-map-join))
   (test-case "v3 sk/fk-bind-return map/join == bind" (check-sk/fk-mapjoin-bind derivation-3 'sk/fk-bind-return))
   (test-case "v2 sk/fk-unit-map-join preserves dk" (check-sk/fk-dk-join derivation-2 'sk/fk-unit-map-join))
   (test-case "v2 sk/fk-bind-return preserves dk" (check-sk/fk-dk-join derivation-2 'sk/fk-bind-return))
   (test-case "v3 sk/fk-unit-map-join preserves dk" (check-sk/fk-dk-join derivation-3 'sk/fk-unit-map-join))
   (test-case "v3 sk/fk-bind-return preserves dk" (check-sk/fk-dk-join derivation-3 'sk/fk-bind-return))
   (test-case "v4 l/m run prefix from examples-new" (check-v4-l/m-run-prefix))))

(module+ main
  (define failures (run-tests derivation-invariant-tests))
  (when (positive? failures)
    (error 'derivation-invariant-regressions (format "~a failure(s)" failures))))
