
(define (next val) 
  (if (even? val)
    (+ val 1)
    (+ val 2)))
(define (smallest-divisor n)
  (define (find-divisor n d)
    (cond ((> (* d d) n) n)
	  ((= (remainder n d) 0) d)
	  (else (find-divisor n (next d)))))
  (find-divisor n 2)
  )

(define (prime? n)
  (= (smallest-divisor n) n))



(define (search-for-primes a)
  (define (iter current result)
    (cond ((>= (length result) 3) (write-line result))
	  ((prime? current) (iter (+ current 2) (append result (list current))))
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

; after
; 100000000000 -> .42
; 1000000000000(1000000000039 1000000000061 1000000000063)          -> 1.29
; 10000000000000(10000000000037 10000000000051 10000000000099)      -> 5.22
; 100000000000000(100000000000031 100000000000067 100000000000097)  -> 16.77

;   (/ 16.77 25.82) 
; => 0.6494965143299768
;    (/ 5.22 7.92) 
; => 0.6590909090909091
;    (/ 1.29 1.97) 
; => 0.6548223350253808
;    (/ 0.42 0.66) 
; => 0.6363636363636364


; it saves about 35% because it can't save time for even numbers
; so previously time cost: a + b
; now                      50%a + b
; [0.5a / (a + b)]
