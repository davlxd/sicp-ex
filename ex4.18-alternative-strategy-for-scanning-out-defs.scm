(define (add-streams s1 s2) (stream-map + s1 s2))
(define (scale-stream stream factor) (stream-map (lambda (x) (* x factor)) stream))
(define (display-stream s n)
  (define (stream-for-each proc s n)
    (if (or (stream-null? s) (= n 0))
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s) (- n 1)))))
  (stream-for-each (lambda (x) (newline) (display x)) s n))

(define identity (lambda (x) x))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)



(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(display-stream (solve identity 1 0.01) 10)


(define (solve2 f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))
(display-stream (solve2 identity 1 0.01) 10)


(define (solve3 f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))

(display-stream (solve3 identity 1 0.01) 10)


; solve3 won't work because for line (b (stream-map f y)),
; evaluator will eval y, which for now is '*unassigned*
;

