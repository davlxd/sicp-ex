(define (display-line x) (newline) (display x))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define even (cons-stream 0 (add-streams even twos)))
(define odd (cons-stream 1 (add-streams odd twos)))
(define ones (cons-stream 1 ones))
(define twos (cons-stream 2 ones))
(define identity (lambda (x) x))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (display-stream s n)
  (define (stream-for-each proc s n)
    (if (or (stream-null? s) (= n 0))
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s) (- n 1)))))
  (stream-for-each (lambda (x) (newline) (display x)) s n))


(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< (weight s1car) (weight s2car))
                   (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                  ((> (weight s1car) (weight s2car))
                   (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                  (else
                    (cons-stream s1car
                                 (cons-stream s2car
                                              (merge-weighted (stream-cdr s1)
                                                              (stream-cdr s2) weight)))))))))

(define (pairs-weighted s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs-weighted (stream-cdr s) (stream-cdr t) weight)
      weight)))


(define (square-sum-of-pairs p) (+ (square (car p)) (square (cadr p))))
(define square-sum-stream (pairs-weighted integers integers square-sum-of-pairs))
(display-stream square-sum-stream 20)


(define (ramanujan s count)
  (if (= count 0)
    'done
    (begin
      (let ((a (stream-car s))
            (b (stream-ref s 1))
            (c (stream-ref s 2))
            (weight-a (square-sum-of-pairs (stream-car s)))
            (weight-b (square-sum-of-pairs (stream-ref s 1)))
            (weight-c (square-sum-of-pairs (stream-ref s 2))))
        (if (= weight-a weight-b weight-c)
          (begin
            (newline)
            (write-line (list weight-a a b c))
            (ramanujan (stream-cdr s) (- count 1)))
          (ramanujan (stream-cdr s) count))))))

; same as previous, will print consecutive-same-weight lines instead of aggregation for more than 3 ways
; like 1105, 1625
(ramanujan square-sum-stream 20)

