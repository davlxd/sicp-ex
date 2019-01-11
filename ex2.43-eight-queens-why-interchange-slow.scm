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
    (print k)
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
	;; qc4 expand to (flatmap (map logic 4) (qc3))
	;; -> (flatmap (map logic 4) ( (flatmap (map logic 4) (qc2)) ))
	;; -> (flatmap (map logic 4) ( (flatmap (map logic 4) ( (flatmap (map logic 4) (qc1)) )) ))
	;; -> (flatmap (map logic 4) ( (flatmap (map logic 4) ( (flatmap (map logic 4) ( (flatmap (map logic 4) (qc0)) )) )) ))
	;; then reduce:
	;; -> (flatmap (map logic 4) ( (flatmap (map logic 4) ( (flatmap (map logic 4) ( (flatmap (map logic 4) ([[]])) )) )) ))
	;; -> (flatmap (map logic 4) ( (flatmap (map logic 4) ( (flatmap (map logic 4) ([...result-of-qc1...])) )) ))
	;; -> (flatmap (map logic 4) ( (flatmap (map logic 4) ([...result-of-qc2...])) ))
	;; -> (flatmap (map logic 4) ([...result-of-qc3...]))
	;; -> [...result-of-qc4...]

	;  (flatmap
	;   (lambda (new-row)
	;    (map (lambda (rest-of-queens)
	;            (adjoin-position new-row k rest-of-queens))
	;          (queen-cols (- k 1))))
	;   (enumerate-interval 1 board-size))
	;; qc4 expand to (flatmap (map (logic qc3)) 4)
	;; -> (flatmap (map (logic (flatmap (map (logic qc2)) 4))) 4)
	;; -> (flatmap (map (logic (flatmap (map (logic (flatmap (map (logic qc1)) 4))) 4))) 4)
	;; -> (flatmap (map (logic (flatmap (map (logic (flatmap (map (logic (flatmap (map (logic qc0)) 4))) 4))) 4))) 4)
	;; then reduce:
	;; -> (flatmap (map (logic (flatmap (map (logic (flatmap (map (logic   (flatmap (map (logic [[]])) 4)   )) 4))) 4))) 4)  
	;;    => qc0-eval 
	;; -> (flatmap (map (logic (flatmap (map (logic (flatmap (map (logic [...result-of-qc1...])) 4))) 4))) 4) 
	;;    => qc1 => 4 x ((qc0-eval) + adjoin-of-qc0)
	;; -> (flatmap (map (logic (flatmap (map (logic [...result-of-qc2...])) 4))) 4) 
	;;    => qc2 => 4 x (qc1-eval + adjoin-of-qc1) => 4 x ((4 x ((qc0-eval) + adjoin-of-qc0)) + adjoin-of-qc1)
	;; ...
	;; ...
	;; so, to make it simple
	;; qc4 expand to 4 x qc3
	;; qc3 expand to 4 x qc2, so so far => 4 x (4 x qc2)
	;; qc2 expand to 4 x qc1, so so far => 4 x (4 x (4 x qc1))
	;; qc1 expand to 4 x qc0, so so far => 4 x (4 x (4 x (4 x qc0)))

	;; or:
	;;  qc4 will eval once, which is FOUR x qc3, which means
	;;  qc3 will eval FOUR times, which is 0100 x qc2, which means
	;;  qc2 will eval FOUR x 0100 times, which is IV x qc1, which means
	;;  qc1 will eval FOUR x 0100 x IV times, which is 4 x qc0, which means
	;;  qc0 will eval FOUR x 0100 x IV x 4 times, which is just [ [] ]


	)))

  (queen-cols board-size))


(queens 4)  

