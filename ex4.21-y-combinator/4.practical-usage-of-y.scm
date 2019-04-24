; Adaptions from That About Wraps it Up: Using FIX to Handle Errors Without Exceptions, and Other Programming Tricks
; By Bruce J. McAdam
; Given we have 
;
(define incomplete-factorial
  (lambda (f)
    (lambda (n)
      (if (= n 1)
        1
        (* n (f (- n 1)))))))

;With Y 
(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (self)
       (f (lambda (x) ((self self) x)))))))


((Y incomplete-factorial) 4)

; We can modify Y to reveal intermidiate values:
(define Y-inspector
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (self)
       (f (lambda (x)
            (let ((ret ((self self) x)))
              (newline)
              (write-line ret)
              ret)
            ))))))

((Y-inspector incomplete-factorial) 5)


; Or write a wrapper without modifying Y itself
;
(define wrapper
  (lambda (f1)
    (lambda (f2)
      (f1 
        (lambda (x) 
          (let ((ret (f2 x)))
            (newline)
            (write-line (list "param" x "ret" ret))
            ret))))))
; Wrapper2
(define wrapper2
  (lambda (being-wrapped)
    (lambda (f)
      (lambda (n)
        (let ((ret ((being-wrapped f) n)))
          (newline)
          (write-line (list "param" n "ret" ret))
          ret)
        ))))

((Y (wrapper incomplete-factorial)) 4)

((Y (wrapper2 incomplete-factorial)) 4)

;
; With intermidiate value and param available, so we can do memoisation for Fib 
;

((Y
   (wrapper
     (lambda (f)
       (lambda (n)
         (cond ((= n 0) 0)
               ((= n 1) 1)
               (else (+ (f (- n 2)) (f (- n 1)))))))))
 5)




