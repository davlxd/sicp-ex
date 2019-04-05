(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))


(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

; vs

(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))



; Hand simulation comes:
;
;
; with local var guesses
;
; guesses
; => (1.0 . #[promise 01]), #[promise 01] is delayed (stream-map proc guesses)
;
; to eval #[promise 01]
;    => (stream-map proc guesses)
;    => ((proc (stream-car guesses)) . #[promise 02]), #[promise 02] is delayed (stream-map proc (stream-cdr guesses))
;                                                                            or (stream-map proc (eval #[promise 01]))
;       => (stream-car guesses) is 1.0
;    => ((proc 1.0) . #[promise 02])
; => (1.5 . #[promise 02])
;
; to eval #[promise 02]
;    => (stream-map proc (stream-cdr guesses))
;    (stream-cdr guesses) -> (stream-cdr (1.0 . #[promise 01])) -> eval #[promise 01] which is CACHED
;    => (stream-map proc (1.5 . #[promise 02]))
;    => ((proc (stream-car (1.5 . #[promise 02]))) . #[promise 03]), #[promise 03] is delayed (stream-map proc (stream-cdr (1.5 . #[promise 02])))
; => (1.4 . #[promise 03])
;
; to eval #[promise 03]
; ...
;

; without local var guesses
; (sqrt-stream 2)
; => (1.0 . #[promise 01]), #[promise 01] is delayed (stream-map proc (sqrt-stream 2))
;
; to eval #[promise 01]
;   => (stream-map proc (sqrt-stream 2))
;   => (stream-map proc (1.0 . #[promise 02])), #[promise 02] is delayed (stream-map proc (sqrt-stream 2))
; => (1.5 . #[promise 03]), #[promise 03] is delayed (stream-map proc (stream-cdr (1.0 . #[promise 02])))
;
; to eval #[promise 03]
;   => (stream-map proc (stream-cdr (1.0 . #[promise 02])))
;   to eval #[promise 02]
;      => (stream-map proc (sqrt-stream 2))
;      => (stream-map proc (1.0 . #[promise 04])), #[promise 04] is delayed (stream-map proc (sqrt-stream 2))
;   => (1.5 . #[promise 05]), #[promise 05] is delayed (stream-map proc (stream-cdr (1.0 . #[promise 04])))
;   => (stream-map proc (1.5 . #[promise 05]))
; => (1.4 . #[promise 06]), #[promise 06] is delayed (stream-map proc (stream-cdr (1.5 . #[promise 05])))
;
; to eval #[promise 06]
; ...
;   
;   

; No memoization looks same as no-local-var
; because for no-local-var, cdr part of stream is always recursively back to (stream-map proc (sqrt-stream 2))
; for no-memoization, eval of intermediate promise will be resursive to (stream-map proc guesses) as well
;


