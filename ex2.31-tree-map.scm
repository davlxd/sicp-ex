; Exercise 2.31.  Abstract your answer to exercise 2.30 to produce a procedure tree-map with the property that square-tree could be defined as

(define (square x) (* x x))

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



(define (tree-map proc tree)
  (map (lambda (item)
	 (if (pair? item)
	   (tree-map proc item)
	   (proc item)))
       tree))

(define (square-tree3 tree) (tree-map square tree))

(square-tree3
  (list 1
	(list 2 (list 3 4) 5)
	(list 6 7)))

