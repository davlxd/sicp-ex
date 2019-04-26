; With applicative-order
; (* n (factorial (- n 1))) needs to be evaled first
; and that leads to eval off (factorial (- n 1)) with n = 5
; which leads to eval of unless with n = 4
; which leads to eval of (factorial (- n 1)) n = 4
; ...
; even with n = 1, (* 1 (factorial (- 1 1))) still gets evaled
; which leads to eval off (factorial 0) 
; ...
; infinite resursion



; It will work with normal-order evaluator



; Verify:

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

(factorial 5)

