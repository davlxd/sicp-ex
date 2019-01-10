(define (fib n)
  (define (iter a b counter)
    (if (= counter 0) 
      a
      (iter b (+ a b) (- counter 1))))
  (iter 0 1 n))

(fib 8)

(define (square x) (* x x))

(define (fib2 n)
  (fib2-iter 1 0 0 1 n))
(define (fib2-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib2-iter a
		    b
		    (+ (square p) (square q))      ; compute p'
		    (+ (* 2 p q) (square q))      ; compute q'
		    (/ count 2)))
	(else (fib2-iter (+ (* b q) (* a q) (* a p))
			 (+ (* b p) (* a q))
			 p
			 q
			 (- count 1)))))

(fib2 8)
