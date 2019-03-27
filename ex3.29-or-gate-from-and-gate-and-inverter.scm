(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a b)
  (if (and
        (or (= a 0) (= a 1))
        (or (= b 0) (= b 1)))
    (* a b)
    (error "Invalid signal" (list a b))))





(define (or-gate a1 a2 output)
  (let ((invert-of-a1 (make-wire))
        (invert-of-a2 (make-wire))
        (and-of-inverted-a1-a2 (make-wire))
        (invert-of-and (make-wire)))
    (inverter a1 invert-of-a1)
    (inverter a2 invert-of-a2)
    (and-gate invert-of-a1 invert-of-a2 and-of-inverted-a1-a2)
    (inverter and-of-inverted-a1-a2 output)))


; the delay would be one inverter-delay plus one and-gate-delay plus another inverter-delay

