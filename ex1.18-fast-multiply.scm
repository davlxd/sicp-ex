(define (double x) (+ x x))
(define (halve x) (/ x 2))


(define (f3 a b)
  (define (iter a b value-so-far)
    (cond ((= b 0) value-so-far)
	  ((even? b) (iter (double a) (halve b) value-so-far))
	  (else (iter a (- b 1) (+ value-so-far a)))))
  (iter a b 0))

(f3 3 10)
