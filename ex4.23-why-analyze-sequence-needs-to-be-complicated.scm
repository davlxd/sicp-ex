;

(define (analyze exp)
  exp)

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))


(define (analyze-sequence-alyssa exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))



; For a sequence with only one expression,
; the one in text produces analyzed exp itself
; Alyssa's version, however, produces (lambda (env) (analyzed-exp env))

; For a sequence with 2 expressions,
; the one in text produces 
; (lambda (env) 
;   (analyzed-exp1 env)
;   (analyzed-exp2 env))
; Alyssa's version produces: 
; (lambda (env) 
;   (analyzed-exp1 env)
;   (analyzed-exp2 env))


; For a sequence with 3 expressions,
; (lambda (env)
;   ((lambda (env)
;      (analyzed-exp1 env)
;      (analyzed-exp2 env))
;    env)
;   (analyzed-exp3 env))
; Alyssa's version produces: 
; (lambda (env) 
;   (analyzed-exp1 env)
;   (analyzed-exp2 env)
;   (analyzed-exp3 env))
;


; Rectification: I merely focused on structure and totally missed the point
; Alyssa's version produces a lambda whose body will not be evaled during analyze phase
; And during execute phase, each time evaluator needs to loop over all analyzed procs, then execute
; including condition check
; While the text's version directly produces a nested lambda
;
