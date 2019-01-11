(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner a result))))
  (iter a null-value))


(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
	  ((not (filter a)) (iter (next a) result))
	  (else (iter (next a) (combiner (term a) result)))))
  (iter a null-value))


(define (inc n) (+ n 1))
(define (identity x) x)
(define (square x) (* x x))
(define (remainder a b)
  (define (iter x)
    (if (> (* x b) a) 
      (- a (* (- x 1) b))
      (iter (+ x 1))))
  (iter 1))


(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (and (= n (smallest-divisor n)) (not (= n 1))))

(define (sum-prime-square a b)
  (filtered-accumulate prime? + 0 square a inc b))
(sum-prime-square 1 5) ;38




(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (product-relative-prime n)
  (define (relative-prime? x)
    (= 1 (gcd x n)))
  (filtered-accumulate relative-prime? * 1 identity 1 inc n))
(product-relative-prime 10) ; 189

