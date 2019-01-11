; (define (reverse1 l)
;   (define (iter l rl)
;     (if (null? l)
;          rl
;          (iter (cdr l) (cons (car l) rl))))
;   (iter (cdr l) (list (car l)))
;   ; (iter l '())
; )

; (define (reverse2 l)
;   (if (null? l)
;     l
;     (append (reverse2 (cdr l)) (list (car l)))))

; (reverse2 (list 1 2 3))


(define (deep-reverse1 l)
  (define (iter l rl)
    (cond ((null? l) rl)
	  ((not (pair? l)) l)
	  (else (iter (cdr l) (cons (deep-reverse1 (car l)) rl) ))))
  (iter l (list ))
  )

(deep-reverse1 (list 1 2 (list 10 (list 100 101 1002) 12 13)))


(define (deep-reverse2 l) ;; flat, wrong
  (cond ((null? l) l)
	((not (pair? l)) (list l))
	(else (append (deep-reverse2 (cdr l)) (deep-reverse2 (car l))))))

(deep-reverse2 (list 1 2 (list 10 (list 100 101 1002) 12 13)))


; fix:
(define (deep-reverse2 l) 
  (cond ((null? l) l)
	((not (pair? l)) l)
	(else (append (deep-reverse2 (cdr l)) (list (deep-reverse2 (car l)))))))

; (deep-reverse2 (list 1 2 (list 10 (list 100 101 1002) 12 13)))
(deep-reverse2 (list 1 2 (list 10 (list 100 101 1002) 12 13)))

