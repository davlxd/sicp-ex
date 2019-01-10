
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


(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (* (expmod base (/ exp 2) m)
		       (expmod base (/ exp 2) m))
		    m))
	(else
	  (remainder (* base (expmod base (- exp 1) m))
		     m))))
;(remainder (square (expmod base (/ exp 2) m)) m)
;(remainder (* base (expmod base (- exp 1) m)) m)
(expmod 2 7 3)
(remainder (* 2 (expmod 2 6 3)) 3)
(remainder (* 2 (remainder (* (expmod 2 3 3) (expmod 2 3 3)) 3)) 3)
(remainder (* 2 (remainder (* (remainder (* 2 (* (remainder (* 2 (expmod 2 0 3)) 3) (remainder (* 2 (expmod 2 0 3)) 3))) 3) (remainder (* 2 (* (remainder (* 2 (expmod 2 0 3)) 3) (remainder (* 2 (expmod 2 0 3)) 3))) 3)) 3)) 3)
(remainder (* 2 (remainder (* (remainder (* 2 (* (remainder (* 2 1) 3) (remainder (* 2 1) 3))) 3) (remainder (* 2 (* (remainder (* 2 1) 3) (remainder (* 2 1) 3))) 3)) 3)) 3)
(remainder (* 2 (remainder (* (remainder (* 2 (* (remainder 2 3) (remainder 2 3))) 3) (remainder (* 2 (* (remainder 2 3) (remainder 2 3))) 3)) 3)) 3)

;(remainder (* 2 (remainder (square (remainder (* 2 (remainder (square (remainder (* 2 (expmod 2 0 3)) 3)) 3)) 3)) 3)) 3)

; Explaination: with square, we'd only calc a branch of calcuations once, then double once
; However with (* ? ?), we'd calc twice, then double once, no calcuation got reduced

