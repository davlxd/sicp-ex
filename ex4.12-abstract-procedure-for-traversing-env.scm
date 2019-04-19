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
            "Unknown procedure type -- NAIVE-APPLY" procedure))))

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
  ; (newline)
  ; (write-line (list "eval" exp))
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'eval (operator exp)) ((get 'eval (operator exp)) exp env))
        ((pair? exp) (naive-apply (eval (operator exp) env)
                                  (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '* *)
        (list 'write-line (lambda (x) (newline) (write-line x)))
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))



; Stub
(define (apply-in-underlying-scheme primitive args) (apply primitive args))

the-global-environment

(eval '(begin
         (define x (cons 10 20))
         (write-line x)
         (
          (lambda () 
            (define x (cons 1 2))
            (write-line x)
            (set! x (* (car x) (cdr x)))
            (write-line x)
            (set! x (cons 100 200))
            (write-line x)
            )
          )
         (write-line x)
         )
      the-global-environment)


; Exercise:
;
;

(define (make-frame variables values) 
  (list (map cons variables values)))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons (cons var val) (car frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan var-val-list)
      (cond ((null? var-val-list)
             (env-loop (enclosing-environment env)))
            ((eq? var (car (car var-val-list)))
             (cdr (car var-val-list)))
            (else (scan (cdr var-val-list)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (car frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan var-val-list)
      (cond ((null? var-val-list)
             (env-loop (enclosing-environment env)))
            ((eq? var (car (car var-val-list)))
             (set-cdr! (car var-val-list) val))
            (else (scan (cdr var-val-list)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((frame (first-frame env)))
        (scan (car frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan var-val-list)
      (cond ((null? var-val-list)
             (add-binding-to-frame! var val frame)
             )
            ((eq? var (car (car var-val-list)))
             (set-cdr! (car var-val-list) val))
            (else (scan (cdr var-val-list)))))
    (scan (car frame))))



; Exercise:
;
(define (traverse-var-in-env var-val define? env)
  (define read? (symbol? var-val))
  (define var (if read? var-val (car var-val)))
  (define val (if read? '() (cdr var-val)))
  (define (env-loop env)
    (define (scan var-val-list frame)
      (cond ((null? var-val-list)
             (if define?
               (add-binding-to-frame! var val frame)
               (env-loop (enclosing-environment env))))
            ((eq? var (car (car var-val-list)))
             (if read? 
               (cdr (car var-val-list))
               (set-cdr! (car var-val-list) val)))
            (else (scan (cdr var-val-list) frame))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (car frame) frame))))
  (env-loop env))
; Honestly this abstraction is ugly

(define (lookup-variable-value var env)
  (traverse-var-in-env var #f env))

(define (set-variable-value! var val env)
  (traverse-var-in-env (cons var val) #f env))

(define (define-variable! var val env)
  (traverse-var-in-env (cons var val) #t env))

(define the-global-environment (setup-environment))
the-global-environment

(eval '(begin
         (define x (cons 10 20))
         (write-line x)
         (
          (lambda () 
            (define x (cons 1 2))
            (write-line x)
            (set! x (* (car x) (cdr x)))
            (write-line x)
            (set! x (cons 100 200))
            (write-line x)
            )
          )
         (write-line x)
         )
      the-global-environment)



