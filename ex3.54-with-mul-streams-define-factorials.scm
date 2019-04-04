(define ones (cons-stream 1 ones))

(define (display-line x) (newline) (display x))

(define (display-stream s n)
  (define (stream-for-each proc s n)
    (if (or (stream-null? s) (= n 0))
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s) (- n 1)))))
  (stream-for-each display-line s n))


(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(display-stream integers 10)


(define factorials (cons-stream 1 (mul-streams
                                    (stream-cdr integers)
                                    factorials
                                    )))

(display-stream factorials 10)

