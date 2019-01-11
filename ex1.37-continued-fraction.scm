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



(define (gr k)
  (/ 1.0
     (cont-frac
       (lambda (i) 1.0)
       (lambda (i) 1.0)
       k)))

; 4 decimal places k = 12
; 1.618055


