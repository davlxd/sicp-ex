(define nil (list))
(define (square x) (* x x))
(define remainder mod)
(define (prime? n)
  (define (divides? a b) (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (= n (smallest-divisor n)))
(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
;; lib ends



(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list j i))
	   (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(unique-pairs 5)


(define (unique-triples s)
  (flatmap
    (lambda (i)
      (flatmap (lambda (j) 
		 (map (lambda (k) (list k j i))
		      (enumerate-interval 1 (- j 1))))
	       (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 s)))

(unique-triples 5)

