
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	  (remainder (* base (expmod base (- exp 1) m))
		     m))))

;(remainder (square (expmod base (/ exp 2) m)) m)
;(remainder (* base (expmod base (- exp 1) m)) m)
(expmod 2 7 3)
(remainder (* 2 (expmod 2 6 3)) 3)
(remainder (* 2 (remainder (square (expmod 2 3 3)) 3)) 3)
(remainder (* 2 (remainder (square (remainder (* 2 (expmod 2 2 3)) 3)) 3)) 3)
(remainder (* 2 (remainder (square (remainder (* 2 (remainder (square (expmod 2 1 3)) 3)) 3)) 3)) 3)
(remainder (* 2 (remainder (square (remainder (* 2 (remainder (square (remainder (* 2 (expmod 2 0 3)) 3)) 3)) 3)) 3)) 3)
(remainder (* 2 (remainder (square (remainder (* 2 (remainder (square (remainder (* 2 1) 3)) 3)) 3)) 3)) 3)
(remainder (* 2 (remainder (square (remainder (* 2 (remainder (square (remainder 2 3)) 3)) 3)) 3)) 3)
(remainder (* 2 (remainder (square (remainder (* 2 (remainder (square 1) 3)) 3)) 3)) 3)
(remainder (* 2 (remainder (square (remainder (* 2 (remainder 1 3)) 3)) 3)) 3)
(remainder (* 2 (remainder (square (remainder (* 2 1) 3)) 3)) 3)
(remainder (* 2 (remainder (square (remainder 2 3)) 3)) 3)
(remainder (* 2 (remainder (square 1) 3)) 3)
(remainder (* 2 (remainder 1 3)) 3)
(remainder (* 2 1) 3)
(remainder 2 3)
2


(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (expmod b n m)
  (remainder (cond ((= n 0) 1)
		   ((even? n) (square (fast-expt b (/ n 2))))
		   (else (* b (fast-expt b (- n 1))))) m))
(expmod 2 7 3)
(remainder (* 2 (fast-expt 2 6)) 3)
(remainder (* 2 (square (fast-expt 2 3))) 3)
(remainder (* 2 (square (* 2 (fast-expt 2 2)))) 3)
(remainder (* 2 (square (* 2 (square (fast-expt 2 1))))) 3)
(remainder (* 2 (square (* 2 (square (* 2 (fast-expt 2 0)))))) 3)
(remainder (* 2 (square (* 2 (square (* 2 1))))) 3)
(remainder (* 2 (square (* 2 (square 2)))) 3)
(remainder (* 2 (square (* 2 4))) 3)
(remainder (* 2 (square 8)) 3)
(remainder (* 2 64) 3)
(remainder 128 3)
2




; I think Alyssa P. Hacker's solution can be faster because she only do remainder once, however the original solution interweave remainder with square and multiply


; answer: 

; The modified version of expmod computes huge intermediate results.
; Scheme is able to handle arbitrary-precision arithmetic, but arithmetic with arbitrarily long numbers is computationally expensive. This means that we get the same (correct) results, but it takes considerably longer.

