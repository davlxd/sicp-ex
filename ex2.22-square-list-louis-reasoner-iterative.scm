(define (square x) (* x x))
; (define (square-list1 items)
;   (if (null? items)
;       (list)
;       (cons (square (car items)) (square-list1 (cdr items)))))

; (define (square-list2 items)
;   (map square items))

; (square-list1 (list 1 2 3 4))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things) 
	    (cons (square (car things))
		  answer))))
  (iter items (list )))

(square-list (list 1 2 3 4))

; Just like Ex2.20, item is processed from the first but prepended to answer


(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
	    (cons answer
		  (square (car things))))))
  (iter items (list )))

(square-list (list 1 2 3 4))

; Just like Ex2.20, this won't work because 1st arg of cons has to be element and 2nd element has to be list, not verse vasa, because according to text scheme only identify resursive pair with last element nil as list

