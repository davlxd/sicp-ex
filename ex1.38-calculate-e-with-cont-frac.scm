(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
      0
      (/ (n i)
	 (+ (d i)
	    (recur (+ i 1))))))
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1)
	    (/ (n i)
	       (+ (d i)
		  result)))))
  ;(recur 1.0))
  (iter k 0.0))


(define (e k)
  (+ 2 
     (cont-frac
       (lambda (i) 1.0)
       (lambda (i)
	 (if (= (remainder (+ i 1) 3) 0)
	   (* (/ (+ i 1) 3) 2)
	   1))
       k)))

; (e 9) = 2.718283582089552

