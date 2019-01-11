(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.00001))
  (define (iter guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	next
	(fixed-point f next))))
  (iter first-guess))


(define (deriv g)
  (define dx 0.00001)
  (lambda (x) 
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g)
	       guess))

(define (newton-method g guess)
  (define (newton-transform g)
    (lambda (x) 
      (- x
	 (/ (g x)
	    ((deriv g) x)))))
  (fixed-point-of-transform g newton-transform guess))


;(define (sqrt x)
;	(newton-method (lambda (y) (- (square y) x)) 1.0))


(define (cubic a b c)
  (lambda (x) (+ (* x x x)
		 (* a (square x))
		 (* b x)
		 c)))

(define (cubic-zero a b c)
  (newton-method (cubic a b c) 1))





