#lang racket

(define make-outer-arg 
  (lambda (s)
    (values (string-append "(" (symbol->string s) " ") ")\n")))

(define (make-inner spc) 
  (match-lambda  
    [`(,s . 0) (format "(~s)~a" s spc)]
    [`(,s . 1) (format "(~s ...)~a" s spc)]
    [`(,s . ,n) (format "(~s ~a...)~a" s (apply string-append (make-list (- n 1) "... ")) spc)]))
;; come back to yak shave here.     
  

(define make-options 
  (let ((other-args '((conj . 2) (disj . 2) (delay . 1) (return . 0) (fail . 0))))
    (for*/list ((first-args '(conj disj))
		(second other-args)
		(third other-args))
      (let-values ([(fst last) (make-outer-arg first-args)])
	(string-append fst ((make-inner " ") second) ((make-inner "") third) last)))))

(printf (apply string-append make-options))



