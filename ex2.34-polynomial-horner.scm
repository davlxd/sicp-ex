(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))

(define (accumulate-with-index op initial sequence)
  (define (iter sequence index)
    (if (null? sequence)
      initial
      (op index
	  (car sequence)
	  (iter (cdr sequence) (+ index 1)))))
  (iter sequence 0)
  )


; For example, to compute 1 + 3x + 5x3 + x5 at x = 2 you would evaluate
; (horner-eval 2 (list 1 3 0 5 0 1))

(define (polynomial base coefficients)
  (accumulate-with-index
    (lambda (index element rest) 
      (+ (* element (expt base index) )
	 rest))
    0
    coefficients))

(polynomial 2 (list 1 3 0 5 0 1))

;1 + 3x^1 + 5x^3 + x^5
;1 + 3x^1 + 0x^2 + 5x^3 + 0x^4 + x^5
;1 + x( 3 + 0x^1 + 5x^2 + 0x^3 + x^4 )
;1 + x( 3 + x(0 + 5x^1 + 0x^2 + x^3) )
;1 + x( 3 + x(0 + x(5 + 0x^1 + x^2)) )
;1 + x( 3 + x(0 + x(5 + x(0 + x^1))) )
;1 + x( 3 + x(0 + x(5 + x(0 + x(1)))) )

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))

