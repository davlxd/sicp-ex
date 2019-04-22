; a. 
((lambda (n) ; -> lambda0
   ((lambda (fact)  ; <- lambda1
      (fact fact n))
    (lambda (ft k)  ; <- lambda2
      (if (= k 1)
        1
        (* k (ft ft (- k 1)))))))
 3)

; Manual emulation:
; => (lambda0 3)
; => (lambda1 lambda2)
; => (lambda2 lambda2 3)
; => (* 3 (lambda2 lambda2 2))
; => (* 3 (* 2 (lambda2 lambda2 1))
; => (* 3 (* 2 1)
;
;
;

; For Fib:
((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fib k)
      (cond ((= k 0) 0)
            ((= k 1) 1)
            (else (+ (fib fib (- k 2)) (fib fib (- k 1))))))))
 9)



; b. 
;
(define (f x)
  (define (even1? n)
    (if (= n 0)
      true
      (odd1? (- n 1))))
  (define (odd1? n)
    (if (= n 0)
      false
      (even1? (- n 1))))
  (even1? x))

(f 4)
(f 3)


(define (f x)
  ((lambda (even1? odd1?) ; <- lambda0
     (even1? even1? odd1? x))
   (lambda (ev? od? n) ; <- lambda1
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n) ; <- lambda2
     (if (= n 0) false (ev? ev? od? (- n 1))))))


(f 4)
(f 3)

; Manual emulation of (f 4)
; => (lambda0 lambda1 lambda2)
; => (lambda1 lambda1 lambda2 4)
; => (lambda2 lambda1 lambda2 3)
; => (lambda1 lambda1 lambda2 2)
; => (lambda2 lambda1 lambda2 1)
; => (lambda1 lambda1 lambda2 0)
; => #t



; The following explores the Y operator/combinitor
;

