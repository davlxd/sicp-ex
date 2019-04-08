(define (display-line x) (newline) (display x))
(define (display-stream s n)
  (define (stream-for-each proc s n)
    (if (or (stream-null? s) (= n 0))
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s) (- n 1)))))
  (stream-for-each display-line s n))

(define (average a b) (/ (+ a b) 2.0))
(define (sqrt-improve guess x)
    (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)


(display-stream (sqrt-stream 2.0) 10)


(define (sqrt x tolerance)
    (stream-limit (sqrt-stream x) tolerance))


(define (stream-limit s tolerance)
  (define (iter a b s)
    (if (<= (abs (- a b)) tolerance)
      b
      (iter b (stream-car s) (stream-cdr s))))
  (iter (stream-car s) (stream-car (stream-cdr s)) (stream-cdr (stream-cdr s))))

; stream-ref 0 and stream-ref 1 can be neater

(sqrt 2 1.0)
(sqrt 2 0.1)
(sqrt 2 0.000001)

