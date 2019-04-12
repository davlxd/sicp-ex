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

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)



(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))

    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams
                  (scale-stream vC (/ 1 L))
                  (scale-stream iL (/ (* -1 R) L))))

    (cons vC iL)))

(define RLC1 (RLC 1 1 0.2 0.1))

(define vC-and-iL (RLC1 10 0))
(display-stream (car vC-and-iL) 10)
(display-stream (cdr vC-and-iL) 10)



