;Give all possible values of x that can result from executing

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))


;1000000:    P1 sets x to 100 and then P2 sets x to 1000000
;1000000:    P2 sets to 1000 and then P1 sets x to x times x into 1000000

;10000:    P2 changes x from 10 to 1000 between the two times that P1 accesses the value of x during the evaluation of (* x x).

;10000:    P1 changes x from 10 to 100 between the three times that P1 accesses the value of x during the evaluation of (* x x x).
;100000:    P1 changes x from 10 to 100 between the three times that P1 accesses the value of x during the evaluation of (* x x x).

;1000:     P2 accesses x (three times), then P1 sets x to 100, then P2 sets x.
;100:    P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.



;Which of these possibilities remain if we instead use serialized procedures:

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

; Only the first two, whose values are all 1000000
;
