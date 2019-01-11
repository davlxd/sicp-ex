(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
	    (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(fold-right op initial (cdr sequence)))))


(define (reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x))) (list ) sequence))
(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) (list ) sequence))


(reverse2 (list 1 2 3 4))


