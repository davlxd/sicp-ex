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
  ; (newline)
  ; (write-line (list "eval-if" exp))
  ; (if (true? (eval (if-predicate exp) env))    ; CHANGE 5
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


(define (eval exp env)
  ; (newline)
  ; (write-line (list "EVAL" exp))
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ; ((application? exp)                  
        ;  (apply (eval (operator exp) env)
        ;         (list-of-values (operands exp) env) env ))    ; CHANGE 1
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
          (error "Unknown expression type -- EVAL" exp))))

(define apply-in-underlying-scheme apply)

(define (apply procedure arguments env)    ; CHANGE 6
  ; (newline)
  ; (write-line (list "apply" procedure))
  (cond ((primitive-procedure? procedure)
         ; (apply-primitive-procedure procedure arguments))
         (apply-primitive-procedure procedure (list-of-arg-values arguments env)))    ; CHANGE 2
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             ; arguments    ; CHANGE 3
             (list-of-delayed-args arguments env)
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type -- APPLY" procedure))))





;
; env
;
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

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

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '* *)
        (list '+ +)
        (list '- -)
        (list '= =)
        (list 'map map)
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
    initial-env))
(define the-global-environment (setup-environment))

;
;
;



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
(define (force-it obj)
  (if (thunk? obj)
    (actual-value (thunk-exp obj) (thunk-env obj))
    obj
    ))


(define (delay-it exp env)
  ; (newline)
  ; (write-line (list "delay-it" exp))
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))



; part a.
(define (execution-time proc)
  (let ((start-ts (real-time-clock)))
    (proc)
    (- (real-time-clock) start-ts)))



; Normal resursive won't work because each time a new frame with new binding is set up

; No memoization
(execution-time
  (lambda () 
    (actual-value
      ; (eval
      '(begin
         (define (square x) (* x x))
         (define (fib n)
           (cond ((= n 0) 0)
                 ((= n 1) 1)
                 (else (+ (fib (- n 2)) (fib (- n 1))))))
         (square (fib 20))
         )
      the-global-environment)
    )
  )
; On my machine 51093

; With memoization
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                         (thunk-exp obj)
                         (thunk-env obj))))
           ; (newline)
           ; (write-line (list "FORM THUNK Memoization from" (list-head obj 2)))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           ; (write-line (list "GET" obj))
           result))
        ((evaluated-thunk? obj)
         (begin
           ; (write-line (list "HIT" obj))
           (thunk-value obj)
           )
         )
        (else obj)))

(execution-time
  (lambda () 
    (actual-value
      ; (eval
      '(begin
         (define (square x) (* x x))
         (define (fib n)
           (cond ((= n 0) 0)
                 ((= n 1) 1)
                 (else (+ (fib (- n 2)) (fib (- n 1))))))
         (square (fib 20))
         )
      the-global-environment)
    )
  )
; On my machine 4493










; Previously I thought Thunk Memoization won't play a major role on plain fib
; because each (fib n) has its own frame and there's no way for consequent fib
; to reuse previously calculated fib
; However Memoization still speed up (fib 20) around 5 times!
; How does this happen?
;
; I inspect memoization construct and utilization with a smaller fib index (fib 5)
; It looks memoization only happens with parameter n
;
(define (force-it obj)
  (if (thunk? obj)
    (actual-value (thunk-exp obj) (thunk-env obj))
    obj
    ))

; (fib 20) without memoization
(execution-time
  (lambda ()
    (actual-value
      '(begin
         (define (fib n)
           (cond ((= n 0) 0)
                 ((= n 1) 1)
                 (else (+ (fib (- n 2)) (fib (- n 1))))))
         (fib 20)
         )
      the-global-environment)
    )
  )


; (fib 20) with memoization
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
(execution-time
  (lambda ()
    (actual-value
      '(begin
         (define (fib n)
           (cond ((= n 0) 0)
                 ((= n 1) 1)
                 (else (+ (fib (- n 2)) (fib (- n 1))))))
         (fib 20)
         )
      the-global-environment)
    )
  )








(define count 0)
(define (id x)
  (set! count (+ count 1)))

; part b.

; WITHOUT memoization

(define (square x) (* x x))
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
<response>
; eval (square (id 10))
; (id 10) get delayed
; eval (* (delayed (id 10)) (delayed (id 10)))
;     force the 1st (delayed (id 10))
;     set count to 1
;     returns 10
;     force the 2nd (delayed (id 10))
;     set count to 2
;     returns 10
; get 100

;;; L-Eval input:
count
;;; L-Eval value:
<response>
; 2




; WITH memoization

(define (square x) (* x x))
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
<response>
; eval (square (id 10))
; (id 10) get delayed
; eval (* (delayed (id 10)) (delayed (id 10)))
;     force the 1st (delayed (id 10))
;     set count to 1
;     returns 10
;     force the 2nd, memoized, returns 10 directly
; get 100

;;; L-Eval input:
count
;;; L-Eval value:
<response>
; 1


