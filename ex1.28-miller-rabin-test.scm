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



(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	  (remainder (* base (expmod base (- exp 1) m))
		     m))))

(define (fermat-test n a)
  ; (= (expmod a n n) a))
  (= (expmod a (- n 1) n) 1))


(define (fast-prime? n)
  (define (iter a)
    (cond ((= a 1) true)
	  ((fermat-test n a) (iter (- a 1)))
	  (else false)))
  (iter (- n 1)))


(fast-prime? 561)
(smallest-divisor 561)


; The alternative fermet can already cover Carmichael number: 561, 1105, 1729, 2465, 2821, and 6601
