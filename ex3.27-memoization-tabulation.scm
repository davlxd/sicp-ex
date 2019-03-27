(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib2 (memoize fib))

; I have to complain that drawing environment diagram is a tedious work:
;
; After above definition, we have:


; ___________________________________________________________________________
;
; global env: fib -----------------------------------------------------+
;             memo-fib ----------------------------------+             |
;             memoize -------------------+               |             |
;             memo-fib2-----+            |               |             |
; __________________________|____________|_______________|_____________|_____
;                           |   ^        |   ^           |   ^         v   ^
;                           |   |        |   |           |   |             |
;                           |   |        |   |           |   |         O   O
;                           |   |        |   |           |   |         |
;                           v   |        v   |           v   |       params: n
;                               |            |               |       body: (cond (( ...
;                           O   O        O   O           O   O                   (( ...
;                           |            |               |   
;                ody: (memoize fib)   params: f       body: (memoize ... 
;                                     body: (let ...
;                                                                     
;
;
;
; (memo-fib 3) creates E1 to eval memo-fib into (memorize (lambda (n ....
;
; ___________________________________________________________________________
;
; global env: fib ----------+                                           
;             memo-fib -----+                                           
;             memoize ------+                                           
;             memo-fib2-----+                                           
; __________________________|________________________________________________
;                           |                 ^
;                           |                 |
;                           |             ____|______
;                           |                                           
;                           v              E1:  
;                           O    O           eval memo-fib
;                           O    O        ___________
;                           ...
;                           O    O
;                           O    O
;                         params:
;                         body:
;
;
; Then create E2 to eval (memoize ... into (lambda (x) ..., which is a procedure whose env is E2
;
; ___________________________________________________________________________
;
; global env: fib ----------+                                           
;             memo-fib -----+                                           
;             memoize ------+                                           
;             memo-fib2-----+                                           
; __________________________|________________________________________________
;                           |                 ^                        ^
;                           |                 |                        |
;                           |             ____|______              ____|______
;                           |                                           
;                           v              E1:                      E2: f: (lambda (n) ...
;                           O    O           eval memo-fib              eval memoize into (lambda (x)
;                           O    O        ___________              ___________                 |
;                           ...                                         ^                      |
;                           O    O                                      |                     O  O
;                           O    O                                      |                        |
;                         params:                                       +------------------------+
;                         body:
;
;
; Then create E3 to eval (lambda (x) ... with initial binding x=3
;
; ___________________________________________________________________________
;
; global env: fib ----------+                                           
;             memo-fib -----+                                           
;             memoize ------+                                           
;             memo-fib2-----+                                           
; __________________________|________________________________________________
;                           |                 ^                        ^
;                           |                 |                        |
;                           |             ____|______              ____|______
;                           |                                           
;                           v              E1:                      E2: f: (lambda (n) ...
;                                                                       table: (())
;                           O    O           eval memo-fib              eval memoize into (lambda (x)
;                           O    O        ___________              ___________                 |
;                           ...                                      ^  ^                      |
;                           O    O                                   |  |                     O  O
;                           O    O                                   |  |                        |
;                         params:                                    |  +------------------------+
;                         body:                                      |
;                                                                    |
;                                                                   _|_________
;
;                                                                    E3: x:3
;                                                                        previously-computed-result: ?
;                                                                        eval (lambda (x) with x=3
;                                                                   ___________
;
;
; (let ((result (f x))) ...  creates E4, (f x) creates E5
; In E5 to eval (f 3), will create E6 and E7 to eval (memo-fib 2) and (memo-fib 1)
;
; Too many sorry I give up
;




; For original recursive version of Fib(n), we'd have a resursion tree
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
;
; With memoize at each level, the right half can be eliminated so it becomes into
;
;                                       F4
;                                     /
;                                   F3
;                                  /
;                                F2
;                              /    \
;                             F1     F0







; (memoize fib) will not work because it only memoize the original biggest parameter



