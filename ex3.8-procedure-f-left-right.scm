

(define prev-arg 0)
(define prev-value 0)

(define (f x)
  (if (>= x prev-arg)
    (begin
      (set! prev-arg x)
      (set! prev-value (- 1 prev-value))
      prev-value)
    (begin
      (set! prev-arg x)
      (set! prev-value (- 0 prev-value))
      prev-value))
  )


(+ (f 0) (f 1)) ; 0
(+ (f 1) (f 0)) ; 1
