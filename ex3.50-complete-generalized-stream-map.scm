
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-line x) (newline) (display x))
(define (display-stream s) (stream-for-each display-line s))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ 1 low) high))))


(stream-map (lambda (x) (* 2 x))
            (stream-enumerate-interval 1 10))

(display-stream (stream-map (lambda (x) (* 2 x))
                            (stream-enumerate-interval 1 10)))


(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
             (cons proc (map stream-cdr argstreams))))))



(stream-map (lambda (x y) (+ (* x x) (* y y)))
            (stream-enumerate-interval 1 4)
            (stream-enumerate-interval 10 14))

(display-stream
  (stream-map (lambda (x y) (+ (* x x) (* y y)))
              (stream-enumerate-interval 1 4)
              (stream-enumerate-interval 10 14)))


