; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>=3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.

(define (f1 n)
  (if (< n 3) 
       n
       (+ (* 1 (f1 (- n 1)))
          (* 2 (f1 (- n 2)))
          (* 3 (f1 (- n 3))))))


(define (f n)
  (define (calc n3 n2 n1)
    (+ (* 3 n3) (* 2 n2) n1))
  (define (iter n3 n2 n1 counter)
    (cond ((< n 3) n)
          ((= counter n) n1)
          (else (iter n2 n1 (calc n3 n2 n1) (+ counter 1)))))
  (iter 0 1 2 2))

(define (f-better n)
  (define (calc n3 n2 n1)
    (+ (* 3 n3) (* 2 n2) n1))
  (define (iter n3 n2 n1 counter)
    (if (= counter n) 
        n3
        (iter n2 n1 (calc n3 n2 n1) (+ counter 1))))
  (iter 0 1 2 0))

(print (f 0) " " (f-better 0))
(print (f 1) " " (f-better 1))
(print (f 2) " " (f-better 2))
(print (f 3) " " (f-better 3))
(print (f 4) " " (f-better 4))
(print (f 5) " " (f-better 5))
(print (f 6) " " (f-better 6))
(print (f 7) " " (f-better 7))

