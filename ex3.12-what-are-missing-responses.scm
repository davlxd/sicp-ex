(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(a b c d)
(cdr x)
; <response> :
; (b)

(define w (append! x y))
w
(a b c d)
(cdr x)
; <response>
; (b c d)



