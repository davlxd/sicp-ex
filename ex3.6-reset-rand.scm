(define random-init 1)

(define (rand-update x)
  (modulo (+ (* 6364136223846793005 x) 1442695040888963407) (expt 2 64)))


(define (rand)
  (define x random-init)
  (define (generate)
    (begin
      (set! x (rand-update x))
      x))
  (define (reset y) (set! x y))

  (lambda (arg)
    (cond ((eq? arg 'generate) (generate))
          ((eq? arg 'reset) reset)))
  )


;; To let rand itself contains state, I'd have to initiate rand like this
(define r (rand))
(r 'generate)
(r 'generate)

((r 'reset) 1)
(r 'generate)
(r 'generate)





; To make rand behave like text, it seems we have to extract the state to global scope:
(define x random-init)

(define (rand1 arg)
  (define (generate)
    (begin
      (set! x (rand-update x))
      x))
  (define (reset y) (set! x y))

  (cond ((eq? arg 'generate) (generate))
        ((eq? arg 'reset) reset))
  )


(rand1 'generate)
(rand1 'generate)
((rand1 'reset) 1)
(rand1 'generate)
(rand1 'generate)




;; Correction: Actually there is another better way:

(define rand2
  (let ((x random-init))
    (define x random-init)
    (define (generate)
      (begin
        (set! x (rand-update x))
        x))
    (define (reset y) (set! x y))

    (lambda (arg)
      (cond ((eq? arg 'generate) (generate))
            ((eq? arg 'reset) reset)))
    )
  )

(rand2 'generate)
(rand2 'generate)
((rand2 'reset) 1)
(rand2 'generate)
(rand2 'generate)

; Actually the return of lambda which that access another variable natually gives us closure
; unfortunately MIT-scheme doesn't allow this form
; (define rand2 (define x random-init) (define ... ...) (define ... ...) ...)
;
;
; Another thing to notice is rand2 always gives us the same lambda, because it was DEFINED
rand2
;Value 13: #[compound-procedure 13]
rand2
;Value 13: #[compound-procedure 13]
;
;
; So is rand:
rand
;Value 14: #[compound-procedure 14 rand]
rand
;Value 14: #[compound-procedure 14 rand]
;
;
; However (rand), an execution of a function, creates a new lambda each time:
(rand)
;Value 15: #[compound-procedure 15]
(rand)
;Value 16: #[compound-procedure 16]
(rand)
;Value 17: #[compound-procedure 17]


;
;
; Let's go wild:
;
(define rand3
  (
   (lambda () 
     (define x random-init)
     (define (generate)
       (begin
         (set! x (rand-update x))
         x))
     (define (reset y) (set! x y))

     (lambda (arg)
       (cond ((eq? arg 'generate) (generate))
             ((eq? arg 'reset) reset)))

     )
   )
  )

(rand3 'generate)
(rand3 'generate)
((rand3 'reset) 1)
(rand3 'generate)
(rand3 'generate)

