

(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                                    (s (lambda () (set! x (+ x 1)))))

;
;101:    P1 sets x to 100 and then P2 increments x to 101.
; This one remains because it's serialized behaviour
;
;
;121:    P2 increments x to 11 and then P1 sets x to x times x.
; This one remains because it's serialized behaviour
;
;
;110:    P2 changes x from 10 to 11 between the two times that P1 accesses the value of x during the evaluation of (* x x).
; This one busted because s encapsulates (* x x), and set! x in P2 also encapsulated with s so P2 cannot happen between the two times ....
;
;
;11:     P2 accesses x, then P1 sets x to 100, then P2 sets x.
; This one remains because even set! and eval of x in P2 encapsulated with serializer, set! x in P1 is not
;
;
;100:    P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.
; This one remains because set! x in P1 is not serialized with eval of (* x x)
;
