
; (define (next val) 
;   (if (even? val)
;       (+ val 1)
;       (+ val 2)))
; (define (smallest-divisor n)
;   (define (find-divisor n d)
;     (cond ((> (* d d) n) n)
;           ((= (remainder n d) 0) d)
;           (else (find-divisor n (next d)))))
;   (find-divisor n 2)
; )

; (define (prime? n)
;   (= (smallest-divisor n) n))



(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	  (remainder (* base (expmod base (- exp 1) m))
		     m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))





(define (search-for-primes a)
  (define (iter current result)
    (cond ((>= (length result) 3) (write-line result))
	  ((fast-prime? current 10) (iter (+ current 2) (append result (list current))))
	  (else (iter (+ current 2) result))))
  (if (even? a)
    (iter (+ a 1) (list))
    (iter a (list))))

; (search-for-primes 100)


(define (timed-search-for-primes n)
  (newline)
  (display n)
  (start-search-for-primes n (runtime)))
(define (start-search-for-primes n start-time)
  (if (search-for-primes n)
    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-search-for-primes 100000000000)

; 100000000000(100000000003 100000000019 100000000057)              -> .66
; 1000000000000(1000000000039 1000000000061 1000000000063)          -> 1.97
; 10000000000000(10000000000037 10000000000051 10000000000099)      -> 7.92
; 100000000000000(100000000000031 100000000000067 100000000000097)  -> 25.82

; with Ex1.23 [2 3 5 7 9 ... ]
; 100000000000                                                      -> .42
; 1000000000000(1000000000039 1000000000061 1000000000063)          -> 1.29
; 10000000000000(10000000000037 10000000000051 10000000000099)      -> 5.22
; 100000000000000(100000000000031 100000000000067 100000000000097)  -> 16.77

; with fermet test k=10
; 100000000000(100000000003 100000000019 100000000057)              -> 0
; 1000000000000(1000000000039 1000000000061 1000000000063)          -> 0
; 10000000000000(10000000000037 10000000000051 10000000000099)      -> 0
; 100000000000000(100000000000031 100000000000067 100000000000097)  -> 0

; I don't unerstand why it's O(lg(n)), more like constant
; It's because it's k(10) * expmod
;                             ^
;                             |
;                            lg(n)
