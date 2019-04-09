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
(display-stream (pairs integers0 integers0) 20)




; Maybe it'll be easier if I can draw a 3d version of this in ASCII

; S0T0 || S0T1   S0T2 S0T3
;======++==================
;      || S1T1 | S1T2 S1T3
;      || -----+-----------
;      ||      | S2T2 S2T3
;      ||      |
;      ||      |      S3T3
;
; I draw on paper and it's shape is a triangular cone


; Starting from S0T0U0, it can be decomposed as 
; S0T0U0 it self, and S0TjUk which is interleaved part of (pairs
; and remaining lead by S1T1U1

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (p) (append (list (stream-car s)) p)) (stream-cdr (pairs t u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))


(display-stream (triples integers0 integers0 integers0) 40)




; Pythagorean triples:

(define pythagorean-triples
  (stream-filter (lambda (l)
                   (let ((s (car l))
                         (t (cadr l))
                         (u (caddr l)))
                     (= (square u) (+ (square s) (square t)))))
                 (triples integers integers integers)))



(display-stream pythagorean-triples 5)
; calculation is slow I think due to U increases much faster than T and S
; I think next exercise can make it faster


