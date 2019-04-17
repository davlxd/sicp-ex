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
  ; (newline)
  ; (write-line (list "naive-apply" procedure arguments))
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
(define (primitive-procedure? procedure) (memq procedure '(= + - * < > even? square write-line)))
(define (apply-primitive-procedure procedure arguments) 
  ; (newline)
  ; (write-line (list "apply-primitive-procedure" procedure arguments))
  (cond ((eq? procedure '=) (if (apply = arguments) 'true 'false))
        ((eq? procedure 'even?) (if (apply even? arguments) 'true 'false))
        ((eq? procedure '+) (apply + arguments))
        ((eq? procedure '-) (apply - arguments))
        ((eq? procedure '*) (apply * arguments))
        ((eq? procedure '<) (if (apply < arguments) 'true 'false))
        ((eq? procedure '>) (if (apply > arguments) 'true 'false))
        ((eq? procedure 'write-line) (newline) (apply write-line arguments))
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
(define (define-variable! var value env) (put 'env var value))
(define (set-variable-value! var value env) (put 'env var value))


(define (derive-do exp)
  (let ((body-in-list (cadr exp))
        (cond-exp (cadddr exp)))
    (list
      (make-lambda '()
                   (list
                     (list 
                       'define 
                       'iter 
                       (make-lambda '()
                                    (append body-in-list
                                            (list (list 'if cond-exp (list 'iter) (list 'quote 'done))))))
                     (list 'iter)
                     )))))

(define (install-do)
  (put 'eval 'do (lambda (exp env) (eval (derive-do exp) env)))
  'done)
(install-do)

(define (derive-for exp)
  (let ((termination-presented? (not (null? (cadr (cadr exp)))))
        (initialization-exp-list (car (cadr exp)))
        (termination-exp-list (cadr (cadr exp)))
        (increment-exp-list (caddr (cadr exp)))
        (body (cddr exp)))
    (let ((new-body (cons 'begin
                          (append body
                                  (append increment-exp-list
                                          (list (list 'iter)))))))
      (if termination-presented?
        (set! new-body (list 'if (car termination-exp-list) new-body)))

      (list
        (make-lambda '()
                     (append initialization-exp-list
                             (list
                               (list 'define 'iter (make-lambda '() (list new-body)))
                               (list 'iter))))))))
(define (install-for)
  (put 'eval 'for (lambda (exp env) (eval (derive-for exp) env)))
  'done)
(install-for)


(define (derive-while exp)
  (let ((cond-exp (cadr exp))
        (body-list (cddr exp)))
    (list
      (make-lambda '()
                   (list
                     (list 
                       'define 
                       'iter 
                       (make-lambda '()
                                    (list
                                      (list 'if cond-exp
                                            (cons 'begin
                                                  (append body-list
                                                          (list (list 'iter))))
                                            (list 'quote 'done)
                                            ))))
                     (list 'iter)
                     )))))

(define (install-while)
  (put 'eval 'while (lambda (exp env) (eval (derive-while exp) env)))
  'done)
(install-while)


; Above from ex4.9


; I'd like to allow infix arithmetic expression when there are only 2 operands

(define (operator exp) 
  (if (and (= (length exp) 3)
           (memq (cadr exp) '(+ - * / = < >)))
    (cadr exp)
    (car exp)))

(define (operands exp) 
  (if (and (= (length exp) 3)
           (memq (cadr exp) '(+ - * / = < >)))
    (cons (car exp) (cddr exp))
    (cdr exp)))


(eval '(+ 1 (* 2 3) (4 * 4) 4) '())
(eval '(> 4 1) '())
(eval '(4 > (2 * 3)) '())



