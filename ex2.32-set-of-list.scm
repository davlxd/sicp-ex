; Exercise 2.32.  We can represent a set as a list of distinct elements, and we can represent the set of all subsets of the set as a list of lists. For example, if the set is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition of a procedure that generates the set of subsets of a set and give a clear explanation of why it works:

; (define (map proc items)
;   (if (null? items)
;       nil
;       (cons (proc (car items))
;             (map proc (cdr items)))))

(define (subsets s)
  (if (null? s)
    (list (list ))
    (let ((rest (subsets (cdr s))))
      (append rest (map 
		     (lambda (item) (cons (car s) item)) 
		     rest)))))


; Why it's working:
; the basic logic for each element in the list
; in each iteration, we add the current item to existing subsets to form new subsets, along with old subsets so we have so far complete subsets 


