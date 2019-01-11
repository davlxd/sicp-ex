(define (inc x)
  	(+ x 1))

(define (double g)
  	(lambda (x) (g (g x))))

((double inc ) 4) ; 6

(((double double) inc) 5)
(( (double double) inc) 5)
;  |             |
( ((lambda (x) (double (double x))) inc) 5)
; |                                    |
( (double (double inc)) 5) 
;         |         |
( (double (lambda (x) (inc (inc x)))) 5)
; |                                 |
( (lambda (x) (inc (inc (inc (inc x))))) 5)


(( (double (double double)) inc ) 5)
(( (double (double double) ) inc ) 5)
;          |             |
(( (double (lambda (x) (double (double x))) ) inc ) 5)
;  |                                        |
(( (lambda (x) (double (double (double (double x ))))) inc) 5)
;|                                                        |
( (double (double (double (double inc)))) 5)
;                         |          |
( (double (double (double (lambda (x) (inc (inc x))))  )) 5)
;                 |                                 |
( (double (double (lambda (x) (inc (inc (inc (inc x)))))  )) 5)
; ....


