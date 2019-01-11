; (define x (list (list 1 2) (list 3 4)))

; (fringe x)
; (1 2 3 4)

; (fringe (list x x))
; (1 2 3 4 1 2 3 4)

(define (fringe1 t)
  (define (recur t l)
    (cond ((null? t) l)
	  ((not (pair? t)) (list t))
	  (else (recur (cdr t) (append l (fringe1 (car t)))))))
  (recur t (list ))
  )


(define (fringe2 t)
  (cond ((null? t) t)
	((not (pair? t)) (list t))
	(else (append (fringe2 (car t)) (fringe2 (cdr t))))))

(define x (list (list 1 2) (list 3 4)))

(fringe2 x)
; (1 2 3 4)

(fringe2 (list x x))
; (1 2 3 4 1 2 3 4)
