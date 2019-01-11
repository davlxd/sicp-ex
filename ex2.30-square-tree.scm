; (square-tree
;  (list 1
;        (list 2 (list 3 4) 5)
;        (list 6 7)))
; (1 (4 (9 16) 25) (36 49))



(define (square x) (* x x))

(define (square-tree1 tree)
  (cond ((null? tree) (list ))
	((not (pair? tree)) (square tree))
	(else (cons (square-tree1 (car tree)) (square-tree1 (cdr tree))))))
; (1 (2 3 4))

; (cons 10 (list ))
; (cons (list 10) (list))

(square-tree1
  (list 1
	(list 2 (list 3 4) 5)
	(list 6 7)))

; (define (map proc items)
;   (if (null? items)
;       nil
;       (cons (proc (car items))
;             (map proc (cdr items)))))

(define (square-tree2 tree)
  (map (lambda (element)
	 (if (pair? element)
	   (square-tree2 element)
	   (square element)))
       tree))

(square-tree2
  (list 1
	(list 2 (list 3 4) 5)
	(list 6 7)))

