

(define (adjoin-term term term-list) (cons term term-list))
(define (the-empty-termlist) '())
(define (first-term term-list) (make-term (length term-list) (car term-list))) ;; correction, should be length - 1, but was okay in ex2.91
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))



(first-term (adjoin-term 'ex (adjoin-term 0 (adjoin-term 'asdf (the-empty-termlist)))))
