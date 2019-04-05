(define ones (cons-stream 1 ones))


(define (display-line x) (newline) (display x))

(define (display-stream s n)
  (define (stream-for-each proc s n)
    (if (or (stream-null? s) (= n 0))
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s) (- n 1)))))
  (stream-for-each display-line s n))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))


; a

(define (div-streams s1 s2) (stream-map / s1 s2))

(define coeff-stream (div-streams ones integers))

(define (integrate-series s) (mul-streams coeff-stream s))

; simpler version:
(define (integrate-series s) (div-streams / s integers))


; b

(define exp-series (cons-stream 1 (integrate-series exp-series)))

(display-stream exp-series 10)


(define negate-ones (cons-stream -1 negate-ones))
(define cosine-series (cons-stream 1
                                   (mul-streams negate-ones (integrate-series sine-series ))))
(define sine-series (cons-stream 0
                                 (integrate-series cosine-series)))


(display-stream cosine-series 10)
(display-stream sine-series 10)

