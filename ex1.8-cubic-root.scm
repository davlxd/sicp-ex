
; (define (good-enough? guess x)
;   (< (abs (- (* guess guess guess) x)) 0.00001))

; (define (improve guess x)
;   (/ (+ (/ x (* guess guess)) 
;         (* 2 guess)) 
;       3))

; (define (cbrt-iter guess x)
;   (if (good-enough? guess x)
;       guess
;       (cbrt-iter (improve guess x) x)))

; (define (cbrt x)
;   (cbrt-iter 1.0 x))





(define (good-enough2? prev-guess guess)
    (< (abs (- prev-guess guess)) 0.00001))

(define (improve guess x)
    (/ (+ (/ x (* guess guess)) 
	          (* 2 guess)) 
             3))

(define (cbrt-iter2 prev-guess guess x)
    (if (good-enough2? prev-guess guess)
            guess
	          (cbrt-iter2 guess (improve guess x) x)))

(define (cbrt2 x)
    (cbrt-iter2 1.0 2.0 x))


(cbrt2 -2)
; (cube-root 1) 
;  (cube-root -8) 
;  (cube-root 27) 
;  (cube-root -1000) 
;  (cube-root 1e-30) 
;  (cube-root 1e60) 
; (cbrt2 0.0000001)
