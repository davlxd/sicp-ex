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

;(define (mul-series a b)
;  (define (helper s1 s2)
;    (cons-stream 0
;                 (add-streams (scale-stream s2 (stream-car s1))
;                              (helper (stream-cdr s1) s2))))
;  (stream-cdr (helper a b)))


(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2)
                            )))


(define sin-x-square-plus-cos-x-square (add-streams
                                         (mul-series cosine-series cosine-series)
                                         (mul-series sine-series sine-series)))

(display-stream (mul-series cosine-series cosine-series) 10)
(display-stream (mul-series sine-series sine-series) 10)
(display-stream sin-x-square-plus-cos-x-square 10)
