
; Some idea around text and this ex
;
; For MIT scheme

(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
; produces "Premature reference to reserved name" because MIT scheme implements transformation in text

; If we change the order of a and b
(let ((a 1))
  (define (f x)
    (define a 5)
    (define b (+ a x))
    (+ a b))
  (f 10))
; Then all good because a has ben set! and not **unassigned** any more

; So I believe the truly solution is utilizing delay eval; change them to procedures:
(let ((a 1))
  (define (f x)
    (define (b) (+ (a) x))
    (define (a) 5)
    (+ (a) (b)))
  (f 10))
; This doesn't even require transformation I believe, eval of (define (a) <...>) only construct '(procedure <...>)
; Not eval the body until (a)
; But can be tricky for evaluator to implement this transformation


