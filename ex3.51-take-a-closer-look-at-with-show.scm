(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-line x) (newline) (display x))
(define (display-stream s) (stream-for-each display-line s))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ 1 low) high))))


(stream-map (lambda (x) (* 2 x))
            (stream-enumerate-interval 1 10))

(display-stream (stream-map (lambda (x) (* 2 x))
                            (stream-enumerate-interval 1 10)))

(define (show x) (display-line x) x)

(define (stream-ref s n)
  (write-line s)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))


(define x (stream-map show (stream-enumerate-interval 0 10)))
; => (stream-map show (cons 0 (delay (stream-enumerate-interval 1 10))))
; This would write 0 to console with (display-line)
; Then x itself would be (0 . #[promise ??]) and that promise is delayed (stream-map show (stream-enumerate-interval 1 10))
;                                                                    or: (stream-map show (cons 1 (delay (stream-enumerate-interval 2 10))))

(stream-ref x 5)
; => (stream-ref (stream-cdr x) 4)
; (stream-cdr x) evals (stream-map show (cons 1 (delay (stream-enumerate-interval 2 10))))
; which print 1 and returns (1 . #[promise ??]) and promise is delayed (stream-map show (stream-enumerate-interval 2 10))
; => (stream-ref (1 . #[promise ??]) 4)
; => (stream-ref (stream-cdr (1 . #[promise ??])) 3)
; (stream-cdr (1 . #[promise ??])) evals (stream-map show (stream-enumerate-interval 2 10))
;                                     or:(stream-map show (cons 2 (delay (stream-enumerate-interval 3 10))))
; which print 2 and returns (stream-ref (2 . #[promise ??]) 3) and promise is lazied (stream-map show (stream-enumerate-interval 3 10))
; ... next
; prints 3 and => (stream-ref (3 . #[promise ??]) 2) and promise is delayed (stream-map show (stream-enumerate-interval 4 10))
; ... next
; prints 4 and => (stream-ref (4 . #[promise ??]) 1) and promise is delayed (stream-map show (stream-enumerate-interval 5 10))
; ... next
; prints 5 and => (stream-ref (5 . #[promise ??]) 0) and promise is delayed (stream-map show (stream-enumerate-interval 6 10))
; and returns 5

; so interpreter will print 1, 2, 3, 4, 5 then return value 5

(stream-ref x 7)
; => (stream-ref (stream-cdr x) 6)
; (stream-cdr x) evals (stream-map show (cons 1 (delay (stream-enumerate-interval 2 10))))
; which has been evaled before, so without printing anything, directly
; => (stream-ref (1 . #[promise ??]) 6), promise is delayed (stream-map show (stream-enumerate-interval 2 10))
; (stream-cdr x) evals (stream-map show (cons 2 (delay (stream-enumerate-interval 3 10))))
; however this time the delayed procedure is generated freshly, so this time 2 will be printed

; so interpreter will print 2, 3, 4, 5, 6, 7 then return value 7





; Correction:
; After actually run the code, (stream-ref x 7) prints 6 and 7 only,
; so my understanding was wrong
;
;(stream-ref x 7)
; => (stream-ref (stream-cdr x) 6)
; (stream-cdr x) evals (stream-map show (cons 1 (delay (stream-enumerate-interval 2 10))))
; which has been evaled before, so without printing anything, directly cache hit into (1 . #[promise ??])
; promise is delayed (stream-map show (stream-enumerate-interval 2 10))
; => (stream-ref (1 . #[promise ??]) 6)
; and the promise, delayed (stream-map show (stream-enumerate-interval 2 10)) is part of cached result
; and this cached result itself, has been evaled before above
; and so on ...







