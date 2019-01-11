; (same-parity 1 2 3 4 5 6 7)
(define (reverse l)
  (define (iter l rl)
    (if (null? l)
      rl
      (iter (cdr l) (cons (car l) rl))))
  (iter (cdr l) (list (car l)))
  )

(define (same-parity initial . rest)
  (define (iter initial rest ret)
    (if (null? rest)
      (reverse ret)
      (let ((e (car rest))
	    (rrest (cdr rest)))
	(cond ((and (even? initial) (even? e)) (iter initial rrest (cons e ret)))
	      ((and (odd? initial) (odd? e)) (iter initial rrest (cons e ret)))
	      (else (iter initial rrest ret))))))
  (iter initial rest (list initial)))




; (same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)


; this yes? from solutions is smarter:
(define (same-parity2 initial . rest)
  (define (iter initial rest ret)
    (if (null? rest)
      (reverse ret)
      (let ((e (car rest))
	    (rrest (cdr rest))
	    (yes? (if (even? initial) even? odd?)))
	(cond ((yes? e) (iter initial rrest (cons e ret)))
	      (else (iter initial rrest ret))))))
  (iter initial rest (list initial)))

(same-parity2 1 2 3 4 5 6 7)

