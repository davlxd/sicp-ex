(define (inc x)
  (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated g n)
  (if (= n 1)
    g
    (lambda (x) ((repeated g (- n 1)) (g x)))))



; Manual iteration:
;=============

((repeated square 2) 5) ; 625
;|                 |
( (lambda (x) ((repeated square 1) (square x))) 5)
;              |                 |
( (lambda (x) (square (square x))) 5)


((repeated square 3) 5) ; 390625
;|                 |
( (lambda (x) ((repeated square 2) (square x))) 5)
;              |                 |
( (lambda (x) (  (lambda (x) ((repeated square 1) (square x)))   (square x))) 5)
( (lambda (x) (  (lambda (x) (square (square x)))   (square x))) 5)
( (lambda (x) (  (square (square (square x))))  ) 5)
;=================



; With compose:
(define (repeated g n)
  (if (= n 1)
    g
    (compose (repeated g (- n 1)) g)))

; better:
(define (identity x) x)
(define (repeated g n)
  (if (< n 1)
    identity
    (compose (repeated g (- n 1)) g)))

; With compose iterative:

(define (identity x) x)

(define (repeated g n)
  (define (iter counter result)
    (if (= counter 0)
      result
      (iter (- counter 1) (compose g result))))
  (iter n identity))



