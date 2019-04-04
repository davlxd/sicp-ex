(define ones (cons-stream 1 ones))

(define (display-line x) (newline) (display x))

(define (display-stream s n)
  (define (stream-for-each proc s n)
    (if (or (stream-null? s) (= n 0))
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s) (- n 1)))))
  (stream-for-each display-line s n))


(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))


; to eval fibs in order:
;
; => (0 . #[promise 01]), #[promise 01] is (1 . #[promise 02]), #[promise 02] is (add-stream ...)
; => (1 . #[promise 02])
;
; => to eval #[promise 02])
;   (add-stream (stream-cdr fibs) fibs)
;   ((0 + 1) . delayed (stream-map + (stream-cdr (0 . #[promise 01])) (stream-cdr (1 . #[promise 02]))))
; => (1 . #[promise 03]), #[promise 03] is delayed (stream-map + (stream-cdr (0 . #[promise 01])) (stream-cdr (1 . #[promise 02])))
;
; => to eval #[promise 03]
;   (stream-map + (stream-cdr (0 . #[promise 01])) (stream-cdr (1 . #[promise 02]))))
;   (stream-map + (eval #[promise 01]) (eval #[promise 02]))) 
;   #[promise 01] and #[promise 02] cached
;   (stream-map + (1 . #[promise 02]) (1 . #[promise 03]))
;   ((1 + 1) . delayed (stream-map + (stream-cdr (1 . #[promise 02])) (stream-cdr (1 . #[promise 03])))
; => (2 . #[promise 04]), #[promise 04] is delayed (stream-map + (stream-cdr (1 . #[promise 02])) (stream-cdr (1 . #[promise 03])))
;
; => to eval #[promise 04]
;   (stream-map + (stream-cdr (1 . #[promise 02])) (stream-cdr (1 . #[promise 03]))))
;   (stream-map + (eval #[promise 02]) (eval #[promise 03])))
;   #[promise 02] and #[promise 03] cached
;   (stream-map + (1 . #[promise 03]) (2 . #[promise 04]))
;   ((1 + 2) . delayed (stream-map + (stream-cdr (1 . #[promise 03])) (stream-cdr (2 . #[promise 04])))
; => (2 . #[promise 05]), #[promise 05] is delayed (stream-map + (stream-cdr (1 . #[promise 03])) (stream-cdr (1 . #[promise 04])))
;
;
; and so on
;
; so with cached/memoized delay, addition happened n times for fib(n)
;
; without memoization, #[promise n] requires #[promise n-1] and #[promise n-2]
; and #[promise n-1] and #[promise n-2] in turn requires their precedences, until reach #[promise 01] and #[promise 02]
; so we'd have a tree like ex3.27:
;
;                                       F4
;                                    /      \
;                               F3             F2
;                             /   \          /    \
;                         F2        F1     F1      F0
;                       /   \
;                    F1      F0
;
; the Amout is 1 + 2 + 4 + ..... + ?
;              |<------- n ------->|
;
; which is approx. 2^n
;



