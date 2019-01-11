(define (for-each proc items)
  (if (null? items)
    #t
    ((lambda () (proc (car items)) (for-each proc (cdr items))))  ))

(for-each (lambda (x) (display x)) (list 1 2 3))

