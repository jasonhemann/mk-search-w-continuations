#lang racket

;; Nested Runs

'((lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (y) ((x x) y))))))
  (lambda (!)
    (lambda (n)
      (if (zero? n) 1
          (* n (! (sub1 n)))))))

(lambda (x) x)

(let ((f (lambda (x) x)))
  (if (f #t)
      (if (f 5)
          f
          f)
      f))

(run 1 (q)
     (run 1 (q2)
          (== q 5))
     (== q 6))

;; This query or one like it should be able to return all instantiations of q for which
;; q maintains that polymorphism
(run 1 (q)
(typo
 '_ `(let ((f (lambda (x) (car (list x ,q)))))
       f)
'a-polymorphic-function)

;; Beware: semiunification. 

(run 1 (q) 
  (== (relationalize-output (run 1 (q) (typo '(lambda (x) (add1 x)) t1)))
      (relationalize-output (run 1 (q) (typo '(lambda (x) x) t1)))))
