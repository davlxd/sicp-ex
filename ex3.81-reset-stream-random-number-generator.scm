(define (add-streams s1 s2) (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define identity (lambda (x) x))
(define (scale-stream stream factor) (stream-map (lambda (x) (* x factor)) stream))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (display-stream s n)
  (define (stream-for-each proc s n)
    (if (or (stream-null? s) (= n 0))
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s) (- n 1)))))
  (stream-for-each (lambda (x) (newline) (display x)) s n))

(define (rand-update x)
  (modulo (+ (* 6364136223846793005 x) 1442695040888963407) (expt 2 64)))

(define random-init 42)

(define (random-number-stream request-stream)
  (define the-stream
    (cons-stream random-init
                 (stream-map
                   (lambda (prev m)
                     (cond ((eq? m 'generate) (rand-update prev))
                           ((eq? m 'reset) random-init)))
                   the-stream
                   request-stream)
                 ))
  the-stream)

(define request-stream (cons-stream
                         'generate
                         (cons-stream
                           'generate
                           (cons-stream
                             'reset
                             (cons-stream
                               'generate
                               (cons-stream
                                 'generate '()))))))

;(display-stream request-stream 10)
(display-stream (random-number-stream request-stream) 10)


