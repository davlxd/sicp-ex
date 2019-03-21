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


