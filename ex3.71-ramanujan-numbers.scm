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


(define (cube x) (* x x x))
(define (cube-sum-of-pairs p) (+ (cube (car p)) (cube (cadr p))))
(define cube-sum-stream (pairs-weighted integers integers cube-sum-of-pairs))
(display-stream cube-sum-stream 20)


(define (ramanujan s count)
  (if (= count 0)
    'done
    (begin
      (let ((a (stream-car s))
            (b (stream-ref s 1))
            (weight-a (cube-sum-of-pairs (stream-car s)))
            (weight-b (cube-sum-of-pairs (stream-ref s 1))))
        (if (= weight-a weight-b)
          (begin
            (newline)
            (write-line (list weight-a a b))
            (ramanujan (stream-cdr s) (- count 1)))
          (ramanujan (stream-cdr s) count))))))

(ramanujan cube-sum-stream 20)

; note ramanujan will print consecutive-same-weight lines instead of aggregation for more than 2 ways Ramanujan numbers

