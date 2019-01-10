
; (define (remainder a b)
;   (define (iter x)
;     (if (> (* x b) a) 
;         (- a (* (- x 1) b))
;         (iter (+ x 1))))
;   (iter 1)
; )

(define (smallest-divisor n)
  (define (find-divisor n d)
    (cond ((> (* d d) n) n)
	  ((= (remainder n d) 0) d)
	  (else (find-divisor n (+ d 1)))))
  (find-divisor n 2)
  )

(define (prime? n)
  (= (smallest-divisor n) n))


; (define (search-for-primes a b)
;  (define (iter current)
;    (if (prime? current)
;        (write-line current))
;    (if (< current b)
;        (iter (+ current 2))))
;  (if (even? a)
;      (iter (+ a 1))
;      (iter a))
; )

; (search-for-primes 10 100)




(define (search-for-primes2 a)
  (define (iter current result)
    (cond ((>= (length result) 3) (write-line result))
	  ((prime? current) (iter (+ current 2) (append result (list current))))
	  (else (iter (+ current 2) result))))
  (if (even? a)
    (iter (+ a 1) (list))
    (iter a (list))))

; (search-for-primes2 100)



(define (timed-search-for-primes n)
  (newline)
  (display n)
  (start-search-for-primes n (runtime)))
(define (start-search-for-primes n start-time)
  (if (search-for-primes2 n)
    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-search-for-primes 10000)
