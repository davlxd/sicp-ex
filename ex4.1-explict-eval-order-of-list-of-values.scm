(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)
(define (eval exp env)
  (newline)
  (write-line (list "eval exp" exp))
  exp
  )


(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))




(list-of-values (list "exp1" "exp2" "exp3") "env")
; My MIT-scheme prints exp3 -> exp2 -> exp1,



; Explict left to right:

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first-eval-ret (eval (first-operand exps) env)))
      (cons first-eval-ret
            (list-of-values (rest-operands exps) env)))))

(list-of-values (list "exp1" "exp2" "exp3") "env")




; Explict right to left:

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((rest-eval-ret (list-of-values (rest-operands exps) env)))
      (cons (eval (first-operand exps) env)
            rest-eval-ret))))

(list-of-values (list "exp1" "exp2" "exp3") "env")

;
