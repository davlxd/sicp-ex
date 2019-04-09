
(define (add-streams s1 s2) (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (display-line x) (newline) (display x))
(define (display-stream s n)
  (define (stream-for-each proc s n)
    (if (or (stream-null? s) (= n 0))
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s) (- n 1)))))
  (stream-for-each display-line s n))


(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define integers0 (cons-stream 0 integers))
(display-stream (pairs integers0 integers0) 20)


; By using similar decomposition strategy


(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave (stream-map (lambda (x) (list (stream-car s) x))
                               (stream-cdr t))

                   (stream-map (lambda (x) (list x (stream-car t)))
                               (stream-cdr s)))

      (pairs (stream-cdr s) (stream-cdr t)))))

(define integers0 (cons-stream 0 integers))
(display-stream (pairs integers0 integers0) 20)



