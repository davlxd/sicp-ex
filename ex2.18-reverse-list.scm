(define (reverse1 l)
  (define (iter l rl)
    (if (null? l)
      rl
      (iter (cdr l) (cons (car l) rl))))
  (iter (cdr l) (list (car l)))
  ; (iter l '())
  )

(define (reverse2 l)
  (if (null? l)
    l
    (append (reverse2 (cdr l)) (list (car l)))))

(reverse2 (list 1 2 3))
