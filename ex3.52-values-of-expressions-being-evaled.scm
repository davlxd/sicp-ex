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

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))




(define sum 0)
sum
(define (accum x)
  (set! sum (+ x sum))
  sum)
sum
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
sum
(define y (stream-filter even? seq))
sum
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
sum
(stream-ref y 7)
sum
(display-stream z)
sum




; sum value

;(define sum 0)
; sum = 0
;
;(define (accum x)
;  (set! sum (+ x sum))
;  sum)
; sum = 0
;
;(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; seq = (1 . #[promise 01]) which promise is delayed (stream-map accum (stream-enumerate-interval 2 20))
; sum = 1
;
;(define y (stream-filter even? seq))
; y = (stream-filter even? (1 . #[promise 01]))
; 1 is odd, so
; => (stream-filter even? (stream-cdr (1 . #[promise 01])))
; => (stream-filter even? (eval #[promise 01]))
; to eval #[promise 01]
; => (stream-map accum (stream-enumerate-interval 2 20))
; => (3 . #[promise 02]) which promise 02 is delayed (stream-map accum (stream-enumerate-interval 3 20)), and sum was set to 1 + 2 = 3
; => (stream-filter even? (3 . #[promise 02]))
; 3 is odd, so
; => (stream-filter even? (stream-cdr (3 . #[promise 02])))
; => (stream-filter even? (eval #[promise 02]))
; to eval #[promise 02]
; => (stream-map accum (stream-enumerate-interval 3 20))
; => (6 . #[promise 03]) which promise 03 is delayed (stream-map accum (stream-enumerate-interval 4 20)), and sum was set to 3 + 3 = 6
; 6 is even, so
; => (6 . #[promise 11]) which promise 11 is delayed (stream-filter even? (stream-cdr (6 . #[promise 03])))
;
; so sum = 6
;
;
;(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;                         seq))
; z = (stream-filter (lambda (x) (= (remainder x 5) 0)) (1 . #[promise 01]))
; 1 is false
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (stream-cdr (1 . #[promise 01])))
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (eval #[promise 01])) <- CACHED
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (3 . #[promise 02]))
; 3 is false
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (stream-cdr (3 . #[promise 02])))
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (eval #[promise 02])) <- CACHED
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (6 . #[promise 03]))
; to eval #[promise 03]
; => (stream-map accum (stream-enumerate-interval 4 20))
; => (10 . #[promise 04]) which promise 04 is delayed (stream-map accum (stream-enumerate-interval 5 20)), and sum was set to 6 + 4 = 10
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (10 . #[promise 04]))
; 10 is true
; => (5 . #[promise 12]) which promise 12 is delayed (stream-filter (lambda (x) (= (remainder x 5) 0)) (stream-cdr (10 . #[promise 04])))
;
; so sum = 10
;
;
;(stream-ref y 7)
;
; itv: 1 2 3 4  5  6  7  8  9  10 11 12 13 14  15  16  17  18  19  20
;
; seq: 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210
;          ^ ^        ^  ^        ^  ^          ^   ^
;          | |        |  |        |  |          |   |
;   y      0 1        2  3        4  5          6   7
;
; so return value is 136
; sum was set to last even seq value, which is 136 as well


;
;(display-stream z)
; This will print the stream as is
;
; itv: 1 2 3 4  5  6  7  8  9  10 11 12 13 14  15  16  17  18  19  20
;
; seq: 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210
;            ^  ^           ^  ^            ^   ^               ^   ^
;            |  |           |  |            |   |               |   |
;   z        0  1           2  3            4   5               6   7
;
; and sum set to 210
;





;
; ==============================================================================
;





; if delayed is not cached
; (define sum 0)
; same as above sum = 0
;
; (define (accum x)
;   (set! sum (+ x sum))
;   sum)
; same as above sum = 0

; (define seq (stream-map accum (stream-enumerate-interval 1 20)))
; same as above sum = 1
;
; (define y (stream-filter even? seq))
; same as above sum = 6
;
;(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;                         seq))
; z = (stream-filter (lambda (x) (= (remainder x 5) 0)) (1 . #[promise 01]))
; 1 is false
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (stream-cdr (1 . #[promise 01])))
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (eval #[promise 01]))
; to eval #[promise 01]
; => (stream-map accum (stream-enumerate-interval 2 20))
; => (8 . #[promise 02]) which promise 02 is delayed (stream-map accum (stream-enumerate-interval 3 20)), and sum was set to 6 + 2 = 8
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (8 . #[promise 02]))
; 8 is false
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (stream-cdr (8 . #[promise 02])))
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (eval #[promise 02]))
; to eval #[promise 02]
; => (stream-map accum (stream-enumerate-interval 3 20))
; => (11 . #[promise 03]) which promise 03 is delayed (stream-map accum (stream-enumerate-interval 4 20)), and sum was set to 8 + 3 = 11
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (11 . #[promise 03]))
; 11 is false
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (stream-cdr (11 . #[promise 03])))
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (eval #[promise 03]))
; to eval #[promise 03]
; => (stream-map accum (stream-enumerate-interval 4 20))
; => (15 . #[promise 04]) which promise 04 is delayed (stream-map accum (stream-enumerate-interval 5 20)), and sum was set to 11 + 4 = 15
; => (stream-filter (lambda (x) (= (remainder x 5) 0)) (15 . #[promise 04]))
; 15 is true
; => (15 . #[promise 12]) which promise 12 is delayed (stream-filter (lambda (x) (= (remainder x 5) 0)) (stream-cdr (15 . #[promise 04])))
;
; so sum = 15
;
;
;
; (stream-ref y 7)
; => (stream-ref (stream-cdr (6 . #[promise 11]) 6)
; => (stream-ref (eval #[promise 11]) 6)
; to eval #[promise 11]
;  => (stream-filter even? (stream-cdr (6 . #[promise 03])))
;  => (stream-filter even? (eval #[promise 03]))
;  to eval #[promise 03]
;  => (stream-map accum (stream-enumerate-interval 4 20))
;  => (19 . #[promise 04]) which promise 04 is delayed (stream-map accum (stream-enumerate-interval 5 20)), and sum was set to 15 + 4 = 19
;  => (stream-filter even? (19 . #[promise04]))
;  => 19 is odd, so
;  => (stream-filter even? (stream-cdr (19 . #[promise04])))
;  => (stream-filter even? (eval #[promise04]))
;  => to eval #[promise04]
;  => (stream-map accum (stream-enumerate-interval 5 20))
;  => (24 . #[promise05]) which promise 05 is delayed (stream-map accum (stream-enumerate-interval 6 20)), and sun was set to 19 + 5 = 24
;  24 is even, so
;  => (24 . #[promise 13]) which promise 13 is delayed (stream-filter even? (stream-cdr (24 . #[promise05])))
; => (stream-ref (24 . #[promise 13]) 6)
; => (stream-ref (stream-cdr (24 . #[promise 13])) 5)
; => (stream-ref (eval #[promise 13]) 5)
; to eval #[promise 13]
;  sum would be set once: 24 + 6 = 30
; => (stream-ref (30 . #[promise 14]) 5)
; => (stream-ref (stream-cdr (30 . #[promise 14])) 4)
; => (stream-ref (eval #[promise 14]) 4)
; to eval #[promise 14]
;  sum would be set: 30 + 7 = 37, 37 + 8 = 45, 45 + 9 = 54
; => (stream-ref (54 . #[promise 15]) 4)
; => (stream-ref (stream-cdr (54 . #[promise 15])) 3)
; => (stream-ref (eval #[promise 15]) 3)
; to eval #[promise 15]
;  sum would be set: 54 + 10 = 64
; => (stream-ref (64 . #[promise 16]) 3)
; => (stream-ref (stream-cdr (64 . #[promise 16])) 2)
; => (stream-ref (eval #[promise 16]) 2)
; to eval #[promise 16]
;  sum would be set: 64 + 11 = 75, 75 + 12 = 87, 87 + 13 = 100
; => (stream-ref (100 . #[promise 17]) 2)
; => (stream-ref (stream-cdr (100 . #[promise 17])) 1)
; => (stream-ref (eval #[promise 17]) 1)
; to eval #[promise 17]
;  sum would be set: 100 + 14 = 114
; => (stream-ref (114 . #[promise 18]) 1)
; => (stream-ref (stream-cdr (100 . #[promise 18])) 0)
; => (stream-ref (eval #[promise 18]) 0)
; to eval #[promise 18]
;  sum would be set to: 114 + 15 = 129, 129 + 16 = 145, 145 + 17 = 162
; => (stream-ref (162 . #[promise 19]) 0)
; => return value 162
; sum is 162


;
;
; (display-stream z)
; z is (15 . #[promise 12]) which promise 12 is delayed (stream-filter (lambda (x) (= (remainder x 5) 0)) (stream-cdr (15 . #[promise 04])))
; and sum is 162
;
;
; itv      5    6    7    8    9    10   11   12   13   14   15   16   17   18   19  20
;
; seq 15   167  173  180  188  197  207  218  230  243  257  272  288  305  323  342 362
; sum 162  (same as seq)
;
;     ^               ^                        ^                        ^
; z   |               |                        |                        |
;

; z as showed, sum is 362




; Verify:
; Note there's a catch while implementing the below naive version of steam construction
; that delay and cons-stream cannot be implemented as reguar procedure,
; otherwise when (delay ...) or (cons-stream ...) evaled, <exp> itself will be evaled 
; in the first place, which is not what we want
; We need something simliar to macro
;
; to fix this without macros I inline lambda for the cdr part of stream
;


(define (cons-stream1 a b) (cons a b))
(define (stream-car1 stream) (car stream))
(define (stream-cdr1 stream) (force1 (cdr stream)))
(define (force1 delayed-object) (delayed-object))

(define (stream-for-each1 proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car1 s))
           (stream-for-each1 proc (stream-cdr1 s)))))

(define (display-line x) (newline) (display x))
(define (display-stream1 s) (stream-for-each1 display-line s))

(define (stream-enumerate-interval1 low high)
  (if (> low high)
    the-empty-stream
    (cons-stream1
      low
      (lambda () (stream-enumerate-interval1 (+ 1 low) high)))))

(define (stream-ref1 s n)
  (if (= n 0)
    (stream-car1 s)
    (stream-ref1 (stream-cdr1 s) (- n 1))))

(define (stream-map1 proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream1 (proc (stream-car1 s))
                  (lambda () (stream-map1 proc (stream-cdr1 s))))))

(define (stream-filter1 pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car1 stream))
         (cons-stream1 (stream-car1 stream)
                       (lambda () (stream-filter1 pred
                                                  (stream-cdr1 stream)))))
        (else (stream-filter1 pred (stream-cdr1 stream)))))



(define sum 0)
sum
(define (accum x)
  (set! sum (+ x sum))
  sum)
sum
(define y (stream-enumerate-interval1 1 4))
y
sum

(define x (stream-map1 (lambda (x) (* x 2)) y))
x
sum

(define seq (stream-map1 accum (stream-enumerate-interval1 1 20)))
sum
seq
(define y (stream-filter1 even? seq))
sum
(define z (stream-filter1 (lambda (x) (= (remainder x 5) 0))
                         seq))
sum
(stream-ref1 y 7)
sum
(display-stream1 z)
sum


; I am amazed all my hand calc is correct
;

