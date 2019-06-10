;
;
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
(define (let? exp) (tagged-list? exp 'let))
(define (let->lambda exp)
  (let ((binding-list (cadr exp))
        (body (cddr exp)))
    (cons
      (make-lambda (map car binding-list) body)
      (map cadr binding-list))))


(define (or? exp) (tagged-list? exp 'or))
(define (or->if exp)
  (define (expand predicates)
    (if (null? predicates)
      'false
      (make-if (car predicates)
               'true
               (expand (cdr predicates)))))
  (expand (cdr exp)))

(define (and? exp) (tagged-list? exp 'and))
(define (and->if exp)
  (define (expand predicates)
    (if (null? predicates)
      'true
      (make-if (list 'not (car predicates))
               'false
               (expand (cdr predicates)))))
  (expand (cdr exp)))



;
;
;



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
        (list '< <)
        (list '<= <=)
        (list '> >)
        (list '>= >=)
        (list 'not not)
        (list 'sqrt sqrt)
        (list 'even? even?)
        (list 'memq memq)
        (list 'eq? eq?)
        (list 'integer? integer?)
        (list 'member member)
        (list 'real-time-clock real-time-clock)
        (list 'abs abs)
        (list '= =)
        (list 'map map)
        (list 'list list)
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

(define apply-in-underlying-scheme apply)
;
;
;


;
;
; amb analyze
;

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))
(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                 (cproc env succeed fail2)
                 (aproc env succeed fail2)))
             fail))))
(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2)
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env                        
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                       (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                             proc args succeed fail3))
                         fail2))
             fail))))
(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs) env
                  (lambda (arg fail2)
                    (get-args (cdr aprocs)
                              env
                              (lambda (args fail3)
                                (succeed (cons arg args)
                                         fail3))
                              fail2))
                  fail)))
(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
          (error
            "Unknown procedure type -- EXECUTE-APPLICATION"
            proc))))
;
;
;



(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices) env
                         succeed
                         (lambda ()
                           (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->lambda exp)))
        ((or? exp) (analyze (or->if exp)))
        ((and? exp) (analyze (and->if exp)))
        ((amb? exp) (analyze-amb exp))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))


(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
        (try-again)
        (begin
          (newline)
          (display ";;; Starting a new problem ")
          (ambeval input
                   the-global-environment
                   ;; ambeval success
                   (lambda (val next-alternative)
                     (announce-output output-prompt)
                     (user-print val)
                     (internal-loop next-alternative))
                   ;; ambeval failure
                   (lambda ()
                     (announce-output
                       ";;; There are no more values of")
                     (user-print input)
                     (driver-loop)))))))
  (internal-loop
    (lambda ()
      (newline)
      (display ";;; There is no current problem")
      (driver-loop))))




(driver-loop)



(define (require p) (if (not p) (amb)))


(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (execution-time proc)
  (let ((start-ts (real-time-clock)))
    (write-line (proc))
    (- (real-time-clock) start-ts)))



; Exercise


; Mary Ann Moore's father -> Lorna
; Colonel Downing -> Melissa (of Sir Barnacle Hood)
; Mr. Hall -> Rosalind
; Sir Barnacle Hood -> Gabrielle
; Dr. Parker

; Gabrielle's father owns the yacht that is named after Dr. Parker's daughter

; Who is Lorna's father



; Wired enough my first solution doesn't use the last constraint but sill converge to singular answer
; and not slow
(define (game-of-yacht)
  (let ((lorna-father (amb 'colonel-downing 'mr-hall 'sir-barnacle-hood 'dr-parker))
        (melissa-father (amb 'sir-barnacle-hood))
        (rosalind-father (amb 'mr-moore 'colonel-downing 'sir-barnacle-hood 'dr-parker))
        (gabrielle-father (amb 'mr-moore 'colonel-downing 'mr-hall 'dr-parker))
        (marry-father (amb 'mr-moore)))
    (require (distinct? (list lorna-father melissa-father rosalind-father gabrielle-father marry-father)))
    (list lorna-father melissa-father rosalind-father gabrielle-father marry-father)
    ))

(game-of-yacht)
try-again


; Sorry not singular answer


; Solution 2
; To express `Gabrielle's father owns the yacht that is named after Dr. Parker's daughter',
; I introduce 2 procedures


(define (game-of-father)
  (define (all-fathers) (amb 'mr-moore 'colonel-downing 'mr-hall 'sir-barnacle-hood 'dr-parker)) ;; TODO need to revisit why all-fathers has to be a lambda here
  (define (all-fathers-except father)
    (let ((ret (all-fathers)))
      (require (not (eq? ret father)))
      ret))
  (define (name-of-his-yacht father-name)
    (cond ((eq? father-name 'mr-moore) 'lorna)
          ((eq? father-name 'colonel-downing) 'melissa)
          ((eq? father-name 'mr-hall) 'rosalind)
          ((eq? father-name 'sir-barnacle-hood) 'gabrielle)
          ((eq? father-name 'dr-parker) 'marry))) ;; a little jump here

  (define lorna-father (all-fathers-except 'mr-moore))
  (define melissa-father 'sir-barnacle-hood)
  (define rosalind-father (all-fathers-except 'mr-hall))
  (define gabrielle-father (all-fathers-except 'sir-barnacle-hood))
  (define marry-father 'mr-moore)

  (define (her-father she)
    (cond ((eq? she 'lorna) lorna-father)
          ((eq? she 'melissa) melissa-father)
          ((eq? she 'rosalind) rosalind-father)
          ((eq? she 'gabrielle) gabrielle-father)
          ((eq? she 'marry) marry-father)))

  (require (distinct? (list lorna-father melissa-father rosalind-father gabrielle-father marry-father)))
  (require (eq? (her-father (name-of-his-yacht gabrielle-father)) 'dr-parker))
  (list lorna-father melissa-father rosalind-father gabrielle-father marry-father)
  )


(game-of-father)
try-again



; Solution 3 
; To inline requires to ambs
;
(define (game-of-daughter)
  (define (all-fathers) (amb 'mr-moore 'colonel-downing 'mr-hall 'sir-barnacle-hood 'dr-parker))
  (define (all-fathers-except fathers)
    (let ((ret (all-fathers)))
      (require (not (memq ret fathers)))
      ret))
  (define (name-of-his-yacht father-name)
    (cond ((eq? father-name 'mr-moore) 'lorna)
          ((eq? father-name 'colonel-downing) 'melissa)
          ((eq? father-name 'mr-hall) 'rosalind)
          ((eq? father-name 'sir-barnacle-hood) 'gabrielle)
          ((eq? father-name 'dr-parker) 'marry))) ;; a little jump here

  (define melissa-father 'sir-barnacle-hood)
  (define marry-father 'mr-moore)
  (define lorna-father (all-fathers-except (list 'mr-moore melissa-father marry-father)))
  (define rosalind-father (all-fathers-except (list 'mr-hall lorna-father melissa-father marry-father)))
  (define gabrielle-father (all-fathers-except (list 'sir-barnacle-hood marry-father lorna-father melissa-father rosalind-father)))

  (define (her-father she)
    (cond ((eq? she 'lorna) lorna-father)
          ((eq? she 'melissa) melissa-father)
          ((eq? she 'rosalind) rosalind-father)
          ((eq? she 'gabrielle) gabrielle-father)
          ((eq? she 'marry) marry-father)))

  (require (eq? (her-father (name-of-his-yacht gabrielle-father)) 'dr-parker))
  (list lorna-father melissa-father rosalind-father gabrielle-father marry-father)
  )

(game-of-daughter)
try-again


(execution-time game-of-father) ; ~ 15
(execution-time game-of-daughter) ; ~ 5




;if we are not told that Mary Ann's last name is Moore.

(define (game-of-rich)
  (define (all-fathers) (amb 'mr-moore 'colonel-downing 'mr-hall 'sir-barnacle-hood 'dr-parker))
  (define (all-fathers-except father)
    (let ((ret (all-fathers)))
      (require (not (eq? ret father)))
      ret))
  (define (name-of-his-yacht father-name)
    (cond ((eq? father-name 'mr-moore) 'lorna)
          ((eq? father-name 'colonel-downing) 'melissa)
          ((eq? father-name 'mr-hall) 'rosalind)
          ((eq? father-name 'sir-barnacle-hood) 'gabrielle)
          ((eq? father-name 'dr-parker) 'marry))) ;; a little jump here

  (define lorna-father (all-fathers-except 'mr-moore))
  (define melissa-father 'sir-barnacle-hood)
  (define rosalind-father (all-fathers-except 'mr-hall))
  (define gabrielle-father (all-fathers-except 'sir-barnacle-hood))
  (define marry-father (all-fathers-except 'dr-parker))

  (define (her-father she)
    (cond ((eq? she 'lorna) lorna-father)
          ((eq? she 'melissa) melissa-father)
          ((eq? she 'rosalind) rosalind-father)
          ((eq? she 'gabrielle) gabrielle-father)
          ((eq? she 'marry) marry-father)))

  (require (distinct? (list lorna-father melissa-father rosalind-father gabrielle-father marry-father)))
  (require (eq? (her-father (name-of-his-yacht gabrielle-father)) 'dr-parker))
  (list lorna-father melissa-father rosalind-father gabrielle-father marry-father)
  )


(game-of-rich)
try-again
try-again

; 2 posibilities


