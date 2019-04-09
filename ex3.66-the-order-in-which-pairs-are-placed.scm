
(define (add-streams s1 s2) (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (display-line x) (newline) (display x))
(define (display-stream s n)
  (define (stream-for-each proc s n)
    (if (or (stream-null? s) (= n 0))
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s) (- n 1)))))
  (stream-for-each display-line s n))


(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define integers0 (cons-stream 0 integers))
(display-stream (pairs integers integers) 10)


; It's 2d resursion, quite difficult
; Given
;
; S0T0 || S0T1   S0T2 S0T3
;======++==================
;      || S1T1 | S1T2 S1T3
;      || -----+-----------
;      ||      | S2T2 S2T3
;      ||      |
;      ||      |      S3T3
;
;
; index:      0     1     2     3     4     5     6     7     8     9     10    11    12    13    14    15    16    17    18    19
;     
;     
;             S0T0  S0T1        S0T2        S0T3        S0T4        S0T5        S0T6        S0T7        S0T8        S0T9        S0T10
;                         S1T1        S1T2                    S1T3                    S1T4                    S1T5
;                                                 S2T2                    S2T3                                            S2T4
;                                                                                                 S3T3
;     
; diagonal    ^           ^                       ^                                               ^
; elements:   |           |                       |                                               |
;
;
;
; What I observed:
;
; For a certain range
; S0T? appear every  2 times, except with diagonal S0T0, and the gap after S0T0 is 1, before is 0
; S1T? appear every  4 times, except with diagonal S1T1, and the gap after S1T1 is 2, before is 2
; S2T? appear every  8 times, except with diagonal S2T2, and the gap after S2T2 is 4, before is 6
; S3T? appear every 16 times, except with diagonal S3T3, and the gap after S3T3 is 8, before is 14
;
;
; What's the pattern of 0 2 6 14?
; I shamefully googled:
;
; 0  = 2^1 - 2
; 2  = 2^2 - 2
; 6  = 2^3 - 2
; 14 = 2^4 - 2
;

;                       x is first   x is 2nd           x is others
;                         |            |                  |
;                         v            v                  v

; So the index of S0Tx: 2^1 - 2, (2^1 - 2) + 2^0, [(2^1 - 2) + 2^0] + 2^1 * (x - 1)
;                 S1Tx: 2^2 - 2, (2^2 - 2) + 2^1, [(2^2 - 2) + 2^1] + 2^2 * (x - 2)
;                 S2Tx: 2^3 - 2, (2^3 - 2) + 2^2, [(2^3 - 2) + 2^2] + 2^3 * (x - 3)
;
;
;                         __  t = s :      2^(s+1) - 2
;                        /
; so the index of SsTt: +---  t = s + 1 :  3*2^s - 2
;                        
;                        \__  t > s + 1 :  (2t - 2s + 1)*2^s - 2
; 
;
; and the index itself is the amount of pairs before the element itself
;
;
;

; (1, 100) is S0T99, = 197
; (99, 100) is S98T99, = 3*2^98 - 2
; (100, 100) is S99T99, = 2^100 - 2
;
;






