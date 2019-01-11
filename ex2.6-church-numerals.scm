(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


(add-1 zero)
;->
(lambda (f) (lambda (x) (f (( (lambda (f) (lambda (x) x))  f) x)))) 
;->
(lambda (f) (lambda (x) (f ( (lambda (x) x)  x)))) 
;->
(lambda (f) (lambda (x) (f x))) 

; so ->
(define one (lambda (f) (lambda (x) (f x))))

(add-1 one)
;->
(lambda (f) (lambda (x) (f (  ( (lambda (f) (lambda (x) (f x)))   f)   x))))
;->
(lambda (f) (lambda (x) (f (    (lambda (x) (f x))   x)   )))
;->
(lambda (f) (lambda (x) (f (f x) )))

; so ->
(define two (lambda (f) (lambda (x) (f (f x)))))



; so -> for body of add-1:
(lambda (x) (f    ((n f) x)   )  )
;                           ^
; this is actual add one    |
;                                ^       ^
; this applies prameter          |       |


; +

(define (+ a b)
  ; (lambda (f) (lambda (x) x)))
  ; (lambda (f) (lambda (x)  ((b f) x)     )))
  ; (lambda (f) (lambda (x)  ((a f) ((b f) x))     )))
  (a b))


; for (define (+ a b) (a b))
; let's say + one and two

(a b)
; ->
( (lambda (f) (lambda (x) (f x)))    (lambda (f) (lambda (x) (f (f x))))  )
; ->
(lambda (x) (    (lambda (f) (lambda (x) (f (f x))))      x))
; ->
(lambda (x) (  (lambda (x) (x (x x)))       )  )
; which is wrong



; for (lambda (f) (lambda (x)  ((a f) ((b f) x))     )))
; let's say + one and two

(lambda (f) (lambda (x)  ((a f) ((b f) x))   ))
;->
(lambda (f) (lambda (x)  ((    (lambda (f) (lambda (x) (f x)))     f) ((     (lambda (f) (lambda (x) (f (f x))))        f) x))   ))
;->
(lambda (f) (lambda (x)  (    (  (lambda (f) (lambda (x) (f x)))     f)  (  (  (lambda (f) (lambda (x) (f (f x))))        f)   x))   ))
; ->
(lambda (f) (lambda (x)  (   (lambda (x) (f x))  ( (lambda (x) (f (f x)))  x))   ))
; ->
(lambda (f) (lambda (x)  (   (lambda (x) (f x))  (f (f x))  )   ))
; ->
(lambda (f) (lambda (x)  (  (f (f (f x)) ) )))

; so:
(define (+ a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)) )))



;

