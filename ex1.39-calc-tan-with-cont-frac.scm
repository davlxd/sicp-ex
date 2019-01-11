(define (tan-cf x k)
  (define (numerator i)
    (if (= i 1)
      x 
      (* x x)))
  (define (odd-value i)
    (- (* 2 i) 1))

  (define (recur i)
    (if (> i k)
      0
      (/ (numerator i)
	 (- (odd-value i)
	    (recur (+ i 1))))))

  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1)
	    (/ (numerator i)
	       (- (odd-value i)
		  result)))))
  ; (recur 1))
  (iter k 0))


