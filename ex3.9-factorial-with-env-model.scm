(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

; for this resursive version, after execution of above define, 
; factorial was added as a binding to global env

; Then the execution of 
(factorial 6)
; create a new env beginning with a frame in which n is bound to 6.
; In this new env, we evaluate the body of factorial:
(lambda (n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))
; To eval this combination, we eval the subexpression (if and (factorial
; which forces us to eval 
(factorial 5)
; And it will create a new env beginning with a frame which n is bound to 5
;
; ...
;
; Until
(factorial 1)
; eval to 1
; then recursely return up ....




(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))

; for this iterative version, after execution of above define, 
; factorial and fact-iter were added as bindings to global env

; Then the execution of 
(factorial 6)
; create a new env beginning with a frame in which n is bound to 6.
; In this new env, we evaluate the body of factorial:
(fact-iter 1 1 6)
; which in turn will create another new env in which n is bound to 6
; in this env we eval the body of fact-iter:
(if (> 1 6)
  1
  (fact-iter 1 2 2))
; ...
; ...







