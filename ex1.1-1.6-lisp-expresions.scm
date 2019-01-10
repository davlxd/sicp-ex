; ; Ex 1.2
; (/ (+ 5 
;       4 
;       (- 2 (- 3 (+ 6 (/ 4 5)))) 
;    (* 3 
;       (- 6 2) 
;       (- 2 7)))


; ; Ex1.3
; (define (sum-of-squares x y) 
;   (+ (* x x) (* y y)))
; (define (>= x y)
;   (not (< x y)))

; (define (asdf a b c) 
;   (if (>= a b)
;       (if (>= b c) 
;            (sum-of-squares a b)
;            (sum-of-squares a c))
;       (if (>= a c) 
;            (sum-of-squares b a)
;            (sum-of-squares b c))))

; (asdf 2 1 3)



; Ex1.6
(define (good-enough? guess x)
    (< (abs (- (* guess guess) x)) 0.001))

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))


(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
	          (else else-clause)))

(define (sqrt-iter guess x)
    (new-if (good-enough? guess x)
	              guess
		                (sqrt-iter (improve guess x)
					                        x)))

                ;|
                ;v

(define (sqrt-iter guess x)
    (cond (predicate then-clause)
	          (else else-clause)))

(define (sqrt-iter guess x)
    (cond ((good-enough? guess x) guess)
	          (else (sqrt-iter (improve guess x) x))))

(sqrt-iter 1.0 10)

