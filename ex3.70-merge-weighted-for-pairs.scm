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


(display-stream (merge-weighted odd even identity) 10)


(define (pairs-weighted s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs-weighted (stream-cdr s) (stream-cdr t) weight)
      weight)))


; a
;
(define a (pairs-weighted integers integers (lambda (p) (+ (car p) (cadr p)))))
(display-stream a 20)


;b 

(define not-divisble-by-2-3-5-stream 
  (stream-filter (lambda (i) 
                   (not (or (= (remainder i 2) 0)
                            (= (remainder i 3) 0)
                            (= (remainder i 5) 0)))) integers))

(display-stream not-divisble-by-2-3-5-stream 20)

(define b
  (pairs-weighted 
    not-divisble-by-2-3-5-stream
    not-divisble-by-2-3-5-stream
    (lambda (p)
      (let ((i (car p))
            (j (cadr p)))
        (+ (* 2 i) (* 3 j) (* 5 i j))))))

(display-stream b 20)



