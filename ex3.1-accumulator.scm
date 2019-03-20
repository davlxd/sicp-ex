;(define (make-accumulator n)
  ;(lambda (x)
    ;(begin (set! n (+ n x)) n)))

(define (make-accumulator n)
  (lambda (x)
    (set! n (+ n x)) 
    n))


(define A (make-accumulator 5))
(A 10)
(A 10)
(A -10)

