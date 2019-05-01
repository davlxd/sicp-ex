; Use DD lazy evaluator as baseline
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
  (if (true? (actual-value (if-predicate exp) env))
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
    'false
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


(define (print-procedure object)
  (if (compound-procedure? object)
    (list 'compound-procedure
          (procedure-parameters object)
          (procedure-body object)
          '<procedure-env>)
    object))


(define (handle-arguments-based-on-parameters parameters arguments env)
  (map (lambda (param arg)
         (cond ((tagged-list? param 'lazy) (delay-it arg env))
               ((tagged-list? param 'lazy-memo) (delay-it-memo arg env))
               (else (eval arg env))))
       parameters arguments))

(define (normalize-parameters parameters)
  (map (lambda (param) (if (pair? param) (car param) param)) parameters))

(define apply-in-underlying-scheme apply)

(define (apply procedure arguments env) 
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-delayed-args arguments env)
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
  (put 'eval 'quote (lambda (exp env) (text-of-quotation exp env)))
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
  ; (write-line (list "EVAL" exp))
  ; (write-line env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'eval (operator exp)) ((get 'eval (operator exp)) exp env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
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
))

(define primitive-procedures
  (list
    (list '* *)
    (list '+ +)
    (list '- -)
    (list '= =)
    (list 'map map)
    (list 'newline newline)
    (list 'display display)
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
    (define-variable! '*unassigned* '*unassigned* initial-env)
    (define-variable! '*NULL* '*NULL* initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

(define (let->combination exp)
  (let ((binding-list (cadr exp))
        (body (cddr exp)))
    (cons
      (make-lambda (map car binding-list) body)
      (map cadr binding-list))))

(define (install-let)
  (put 'eval 'let (lambda (exp env) (eval (let->combination exp) env)))
  'done)
(install-let)




;
; lazy library
;

(define (actual-value exp env)
  ; (newline)
  ; (write-line (list "actual-value" exp))
  (force-it (eval exp env)))


(define (list-of-arg-values exps env)
  (if (no-operands? exps)
    '()
    (cons (actual-value (first-operand exps) env)
          (list-of-arg-values (rest-operands exps)
                              env))))
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
    '()
    (cons (delay-it (first-operand exps) env)
          (list-of-delayed-args (rest-operands exps)
                                env))))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (delay-it-memo exp env)
  (list 'thunk-memo exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-memo? obj)
  (tagged-list? obj 'thunk-memo))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                         (thunk-exp obj)
                         (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))
;
;
;




; Exercise

; First convert (list <...>) to (cons <...> (cons <...>)) in evaluator
; Although cons is defined in user space
;
(define (make-cons a b) (list 'cons a b))

(define (list->cons exp)
  (define (recur elements)
    (if (null? elements)
      '*NULL*
      (make-cons (car elements) (recur (cdr elements)))))
  (recur (cdr exp)))

(define (install-list)
  (put 'eval 'null? (lambda (exp env) (if (eq? '*NULL* (eval (cadr exp) env)) 'true 'false)))
  (put 'eval 'list (lambda (exp env) (eval (list->cons exp) env)))
  'done)
(install-list)



(actual-value
  '(begin
     (define (cons x y) (lambda (m) (m x y)))
     (define (car z) (z (lambda (p q) p)))
     (define (cdr z) (z (lambda (p q) q)))

     (define x (cons 1 2))
     (write-line (car x))
     (write-line (cdr x))

     (define y (list 1 2 3 4))
     (write-line (car y))
     (write-line (car (cdr y)))
     (write-line (car (cdr (cdr y))))
     (write-line (car (cdr (cdr (cdr y)))))
     (write-line (cdr (cdr (cdr (cdr y)))))

     (define z (list ))
     (null? z)
     )
  (setup-environment))



;
; Next revise text-of-quotation, if text is parenthetical, then convert to (list <...>) in the evaluator
; (not consider pairs)
;
(define (text-of-quotation exp env)
  (let ((content (cadr exp)))
    (if (list? content) (eval (cons 'list content) env)
      content)))

(actual-value
  '(begin
     (define (cons x y) (lambda (m) (m x y)))
     (define (car z) (z (lambda (p q) p)))
     (define (cdr z) (z (lambda (p q) q)))

     (define y (quote (1 2 3 4)))
     (write-line (car y))
     (write-line (car (cdr y)))
     (write-line (car (cdr (cdr y))))
     (write-line (car (cdr (cdr (cdr y)))))
     (write-line (cdr (cdr (cdr (cdr y)))))
     (null? (quote ()))
     )
  (setup-environment))


