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
(define (div-streams s1 s2) (stream-map / s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))
(define coeff-stream (div-streams ones integers))

(define (integrate-series s) (mul-streams coeff-stream s))



(define (scale-stream stream factor) (stream-map (lambda (x) (* x factor)) stream))

(define negate-ones (cons-stream -1 negate-ones))
(define cosine-series (cons-stream 1
                                   (mul-streams negate-ones (integrate-series sine-series ))))
(define sine-series (cons-stream 0
                                 (integrate-series cosine-series)))
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2)
                            )))
(define (invert-unit-series s)
  (cons-stream 1
               (mul-streams negate-ones
                            (mul-series (stream-cdr s)
                                        (invert-unit-series s)))))


; sln:

(define (div-series s1 s2)
  (if (= 0 (stream-car s2))
    (error "error")
    (mul-series s1
                (invert-unit-series (scale-stream s2 (/ 1 (stream-car s2)))))))


(define invert-of-cos (invert-unit-series cosine-series))
(display-stream invert-of-cos 10)

(define zeros (cons-stream 0 zeros))
(define one-and-zeros (cons-stream 1 zeros))

(display-stream (div-series one-and-zeros cosine-series) 10)


