(define (inc x)
  (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (identity x) x)

(define (repeated g n)
  (define (iter counter result)
    (if (= counter 0)
      result
      (iter (- counter 1) (compose g result))))
  (iter n identity))



(define (smooth f)
  (define dx 0.001)
  (lambda (x) (/ (+ (f (- x dx))
		    (f x)
		    (f (+ x dx)))
		 3)))


(define (some-function-i-forget-name x)
  (if (< x 0) 0 (square x)))

((smooth some-function-i-forget-name ) 0)

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (n-fold-smooth2 f n)
  (repeated (smooth f) n))



((n-fold-smooth some-function-i-forget-name 4) 0)


