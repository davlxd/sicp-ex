
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

(define integers (cons-stream 1 (add-streams ones integers)))

; looks like factorial with +
(define (partial-sums s) (cons-stream (stream-car s)
                                      (add-streams
                                        (stream-cdr s)
                                        (partial-sums s))))

(display-stream (partial-sums integers) 10)



; huntzhan's sln on schemewiki is quite smart 
; considering both (stream-car s) and (stream-cdr s) are appeared in above solutions
(define (partial-sums s) 
     (add-streams s (cons-stream 0 (partial-sums s)))) 

