(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.00001))
  (define (iter guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	next
	(fixed-point f next))))
  (iter first-guess))


(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)


