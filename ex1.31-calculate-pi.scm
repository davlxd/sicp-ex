(define (incr2 x) (+ x 2))
(define (square x) (* x x))

(define (product a times f next)
  (if (= times 0)
    1
    (* (f a) (product (next a) (- times 1) f next))))

(define (product-iter a times f next)
  (define (iter a times result)
    (if (= times 0)
      result
      (iter (next a) (- times 1) (* result (f a)))))
  (iter a times 1))

(define (product-pi-numerator times)
  (if (even? times) 
    (* (/ (+ times 2) 2) (product 2 (/ times 2) square incr2))
    (* 2 (product 4 (/ (- times 1) 2) square incr2))))

(define (product-pi-denominator times)
  (if (even? times) 
    (product 3 (/ times 2) square incr2)
    (* (+ times 2) (product 3 (/ (- times 1) 2) square incr2))))

(define (pi n)
  (* 4
     (/ (product-pi-numerator n) (product-pi-denominator n))))

(product-pi-numerator 1000)
; (pi 1010)
; numerator or denominator is just too big for scheme

; sln2 using pair
(define (product-pi times)
  (define (f p) (/ (car p) (cdr p)))
  (define (next p)
    (if (> (car p) (cdr p))
      (cons (car p) (+ (cdr p) 2))
      (cons (+ (car p) 2) (cdr p))))
  (* 4 (product (cons 2 3) times f next))
  )

(product-pi 100)



