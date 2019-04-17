(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)   ; formal parameters
                 (cddr exp)))) ; body
(define (eval-definition exp env)
  (newline)
  (write-line (list "eval-definition" exp))
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
    'false                          ; no else clause
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF"
                 clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))

(define (naive-apply procedure arguments)
  (newline)
  (write-line (list "naive-apply" procedure arguments))
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))

; Naive table put/get
(define table '())
(define (get . args)
  (let ((target-list (find (lambda (entry) (equal? args (sublist entry 0 (length args)))) table)))
    (if target-list
      (list-ref target-list (length args))
      #f)))
(define (put . args) (set! table (cons args table)))
;

(define (install-eval-mutiple)
  (put 'eval 'quote (lambda (exp env) (text-of-quotation exp)))
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp)
                                                       (lambda-body exp)
                                                       env)))
  (put 'eval 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
  (put 'eval 'cond (lambda (exp env) (eval (cond->if exp) env)))
  'done)

(install-eval-mutiple)


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'eval (operator exp)) ((get 'eval (operator exp)) exp env))
        ((pair? exp) (naive-apply (eval (operator exp) env)
                                  (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))


; Stubs
(define (primitive-procedure? procedure) (memq procedure '(= + - * even? square)))
(define (apply-primitive-procedure procedure arguments) 
  ; (newline)
  ; (write-line (list "apply-primitive-procedure" procedure arguments))
  (cond ((eq? procedure '=) (if (apply = arguments) 'true 'false))
        ((eq? procedure 'even?) (if (apply even? arguments) 'true 'false))
        ((eq? procedure '+) (apply + arguments))
        ((eq? procedure '-) (apply - arguments))
        ((eq? procedure '*) (apply * arguments))
        ((eq? procedure 'square) (apply square arguments))
        ))
(define (true? t) 
  (or 
    (eq? t 'true)
    (and (number? t) (not (= t 0)))
    ))
(define (false? f) (eq? f 'false))

(define (lookup-variable-value exp env) (if (get 'env exp) (get 'env exp) exp))
(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (compound-procedure? procedure) (tagged-list? procedure 'procedure))
(define (procedure-environment a) '())
(define procedure-parameters cadr)
(define procedure-body caddr)
(define (extend-environment vars vals base-env)
  (for-each (lambda (var val) (put 'env var val)) vars vals)
  '())
;
; I believe we need support of define, so more stubs:

(define (define-variable! var value env) (put 'env var value))

;

(eval '(begin 
         (define (triple x) (* x x x))
         (triple 2)
         )
      '())


; Revise let->combination

(define (let->combination exp)
  (let* ((named? (not (pair? (cadr exp))))
         (var (if named? (cadr exp) '()))
         (body ((if named? cdddr cddr) exp))
         (binding-list ((if named? caddr cadr) exp))
         (binding-vars (map car binding-list))
         (binding-vals (map cadr binding-list))
         (the-lambda (make-lambda binding-vars body)))
    (if named?
      (list 'begin
            (list 'define var the-lambda)
            (cons var binding-vals))
      (cons the-lambda binding-vals))))

; Rectification: like ex4.9, the name needs to be hided in a new env frame


(let->combination '(let ((x (square 2))
                         (y (+ 3 4)))
                     (+ x y)
                     (- x y))
                  )
;Value: ((lambda (x y) (+ x y) (- x y)) (square 2) (+ 3 4))

(let->combination '(let fib-iter ((a 1)
                                  (b 0)
                                  (count n))
                     (if (= count 0)
                       b
                       (fib-iter (+ a b) a (- count 1)))))
;Value: (begin (define fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1))))) (fib-iter 1 0 n))


(define (install-let)
  (put 'eval 'let (lambda (exp env) (eval (let->combination exp) env)))
  'done)
(install-let)

(eval '(let fib-iter ((a 1)
                      (b 0)
                      (count 8))
         (if (= count 0)
           b
           (fib-iter (+ a b) a (- count 1))))
      '())
;Value: 21




