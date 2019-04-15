; COPY & PASTE
;
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

(define (apply procedure arguments)
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
        ((pair? exp) (apply (eval (operator exp) env)
                            (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))


; Stubs
(define x 0)
(define (define-variable! a b env)
  (newline)
  (write-line (list "define-variable" a b env))
  (set! x b))




; Exercise begins
;
(define (logic-components exp) (cdr exp))
(define (eval-and exp env)
  (define (iter components)
    (cond ((null? components) 'true)
          ((false? (eval (car components) env)) 'false)
          (else (iter (cdr components)))))
  (iter (logic-components exp)))

(define (eval-or exp env)
  (define (iter components)
    (cond ((null? components) 'false)
          ((true? (eval (car components) env)) 'true)
          (else (iter (cdr components)))))
  (iter (logic-components exp)))

(define (install-eval-and-or)
  (put 'eval 'and eval-and)
  (put 'eval 'or eval-or)
  'done)
(install-eval-and-or)


; stubs
(define (lookup-variable-value exp env) exp)
(define (true? t) (eq? t 'true))
(define (false? t) (eq? t 'false))
;

(eval '(and ) '())
(eval '(or ) '())

(eval '(and true) '())
(eval '(and false) '())

(eval '(or true) '())
(eval '(or false) '())

(eval '(and true true) '())
(eval '(and true false) '())
(eval '(and false true) '())
(eval '(and false false) '())

(eval '(or true true) '())
(eval '(or true false) '())
(eval '(or false true) '())
(eval '(or false false) '())





; And and Or can be derivation of If
;
; (and (> x 0) (> y 0) (> z 0))
; => 
; (if (> x 0)
;   (if (> y 0)
;     (if (> z 0)
;       'true
;       'false)
;     'false)
;   'false)

(define (logic-components exp) (cdr exp))
(define (and->if exp)
  (newline)
  (write-line "Derived version")
  (define (expand-components components)
    (cond ((null? components) 'true)
          (else (make-if (car components)
                         (expand-components (cdr components))
                         'false))))
  (expand-components (logic-components exp)))

; (or (> x 0) (> y 0) (> z 0))
; => 
; (if (> x 0)
;   'true
;   (if (> y 0)
;     'true
;     (if (> z 0)
;       'true
;       'false)
;     'false)
;    'false)

(define (or->if exp)
  (newline)
  (write-line "Derived version")
  (define (expand-components components)
    (cond ((null? components) 'false)
          (else (make-if (car components)
                         'true
                         (expand-components (cdr components))))))
  (expand-components (logic-components exp)))

(define (install-and-or->if)
  (put 'eval 'and (lambda (exp env) (eval (and->if exp) env)))
  (put 'eval 'or (lambda (exp env) (eval (or->if exp) env)))
  'done)
(install-and-or->if)


(eval '(and ) '())
(eval '(or ) '())

(eval '(and true) '())
(eval '(and false) '())

(eval '(or true) '())
(eval '(or false) '())

(eval '(and true true) '())
(eval '(and true false) '())
(eval '(and false true) '())
(eval '(and false false) '())

(eval '(or true true) '())
(eval '(or true false) '())
(eval '(or false true) '())
(eval '(or false false) '())



