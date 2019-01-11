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
(define (map-n op . seqs) ;; BiwaScheme doesn't have generic map
  (define (recur seqs)
    (if (null? (car seqs))
      (list )
      (cons (apply op (map car seqs))
	    (recur (map cdr seqs)))))
  (recur seqs))
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
;; lib ends

;; new lib
(define (enumerate-interval a b)
  (define predicate (if (< a b) > < ))
  (define op (if (< a b) + - ))
  (define (doit a b)
    (if (predicate a b)
      nil
      (cons a (doit (op a 1) b))))
  (doit a b))


(define (queens board-size)
  (define empty-board (list))
  (define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))
  (define (same-row? new-row rest)
    (> (length 
	 (filter (lambda (x) (= x new-row)) 
		 rest))
       0))
  (define (same-diagnal? new-xy rest-xy)
    (> (length 
	 (filter (lambda (each-xy)
		   (let ((each-x (car each-xy))
			 (each-y (cdr each-xy))
			 (new-x (car new-xy))
			 (new-y (cdr new-xy)))
		     (or (= (+ each-x each-y) (+ new-x new-y))
			 (= (- each-x each-y) (- new-x new-y)))))
		 rest-xy))
       0))
  (define (safe? k positions)
    (let ((xy (map-n cons positions (enumerate-interval k 1))))
      (and (not (same-row? (car positions) (cdr positions)))
	   (not (same-diagnal? (car xy) (cdr xy))))))

  (define (queen-cols k)  
    (if (= k 0)
      (list empty-board)
      (filter
	(lambda (positions) (safe? k positions))
	(flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1)))
	)))

  (queen-cols board-size))


(queens 8)  


