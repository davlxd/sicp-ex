(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (f1 a b)
  (if (= b 0)
    0
    (+ a (f1 a (- b 1)))))


(define (f2 a b)
  (cond ((= b 0) 0)
	((even? b) (double (f2 a (halve b))))
	(else (+ a (f2 a (- b 1))))
	)
  )
(f2 3 2)
