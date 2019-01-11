(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

; (sum-integers 1 10)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
	      (accumulate combiner null-value term (next a) next b))))
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum-integers2 a b)
  (sum identity a inc b))
(sum-integers2 1 4); 10

(define (product-integers2 a b)
  (accumulate * 1 identity a inc b))
(product-integers2 1 5) ; 120


(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-integers3 a b)
  (accumulate-iter + 0 identity a inc b))
(sum-integers3 1 4) ;10

(define (product-integers3 a b)
  (accumulate-iter * 1 identity a inc b))
(product-integers3 1 5)

