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
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a b)
  (if (and
        (or (= a 0) (= a 1))
        (or (= b 0) (= b 1)))
    (min 1 (+ a b))
    (error "Invalid signal" (list a b))))




; Wire implementation copied from next section:
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
               (call-each action-procedures))
        'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin
      ((car procedures))
      (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure) ((wire 'add-action!) action-procedure))



; Fake after-delays
(define or-gate-delay 0)
(define and-gate-delay 0)
(define inverter-delay 0)
(define (after-delay delay proc) (proc))



; Testing basic gates:
(let ((a (make-wire))
      (o (make-wire)))
  (inverter a o)
  (set-signal! a 1)
  (newline)
  (write-line (get-signal o))

  (set-signal! a 0)
  (newline)
  (write-line (get-signal o)) )

(let ((a (make-wire))
      (b (make-wire))
      (o (make-wire)))
  (or-gate a b o)
  (set-signal! a 1)
  (newline)
  (write-line (get-signal o)))

(let ((a (make-wire))
      (b (make-wire))
      (o (make-wire)))
  (and-gate a b o)
  (set-signal! a 1)
  (newline)
  (write-line (get-signal o)))




; Adders

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))


(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


; Test full adder

(let ((a (make-wire))
      (b (make-wire))
      (c-in (make-wire))
      (sum (make-wire))
      (c-out (make-wire)))
  (full-adder a b c-in sum c-out)

  (set-signal! a 1)
  (newline)
  (write-line (list (get-signal sum) (get-signal c-out)))

  (set-signal! b 1)
  (newline)
  (write-line (list (get-signal sum) (get-signal c-out)))

  (set-signal! c-in 1)
  (newline)
  (write-line (list (get-signal sum) (get-signal c-out)))
  )



; ripple-carry-adder

(define (ripple-carry-adder A B)
  (define (adder A B)
    (cond ((and (null? A) (null? B)) (cons (make-wire) '())) ;; given make-wire default 0
          ((or (null? A) (null? B)) (error "A and B length not equal" (list A B)))
          (else (let ((c-out (make-wire))
                      (sum (make-wire)))
                  (let ((next-adder-result (adder (cdr A) (cdr B))))
                    (full-adder (car A) (car B) (car next-adder-result) sum c-out)
                    (cons c-out (cons sum (cdr next-adder-result))))))))
  (adder A B))


; Test

(define (make-wire-1)
  (let ((wire (make-wire)))
    (set-signal! wire 1)
    wire))

(let ((A (list (make-wire-1) (make-wire-1)))
      (B (list (make-wire) (make-wire-1))))
  (let ((C-and-S (ripple-carry-adder A B)))
    (map get-signal C-and-S)))


(let ((A (list (make-wire-1) (make-wire-1)))
      (B (list (make-wire-1) (make-wire-1))))
  (let ((C-and-S (ripple-carry-adder A B)))
    (map get-signal C-and-S)))

(let ((A (list (make-wire-1) (make-wire) (make-wire) (make-wire-1) (make-wire) (make-wire-1) ))
      (B (list (make-wire) (make-wire-1) (make-wire) (make-wire-1) (make-wire) (make-wire-1) )))
  (let ((C-and-S (ripple-carry-adder A B)))
    (map get-signal C-and-S)))




; Delay is (n * delay-of-full-adder)
; delay-of-full-adder is (2 * delay-of-half-adder + delay-of-or)
; delay-of-half-adder is (delay-of-add + max(delay-of-or, delay-of-add + delay-of-inverter))
; = 2n * (delay-of-add + max(delay-of-or, delay-of-add + delay-of-inverter)) + n * delay-of-or
;

