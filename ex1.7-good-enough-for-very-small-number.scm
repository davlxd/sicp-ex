; Ex1.7
(define (good-enough? guess x)
    (< (abs (- (* guess guess) x)) (* guess 0.001)))

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (sqrt-iter guess x)
    (if (good-enough? guess x) 
            guess 
	          (sqrt-iter (improve guess x) x)))

(define (sqrt x)
    (sqrt-iter 1.0 x))

; (sqrt 100000000000000)
; (sqrt 999999999999990)
(sqrt 0.00000001)

; For small numbers the number itself may even smaller than difference threshold 0.001, in this case the squered guess could have a big difference with x
; For large numbers, I really can't say

; revised:
; (define (good-enough? prev-guess guess)
;   (< (abs (- prev-guess guess)) 0.001))


; (define (sqrt-iter prev-guess guess x)
;   (if (good-enough? prev-guess guess) 
;       guess 
;       (sqrt-iter guess (improve guess x) x)))

; (define (sqrt x)
;   (sqrt-iter 2.0 1.0 x))

; ; (sqrt 0.00001)
; (sqrt 100000000000000)


