(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


(define (cyclic? c)
  (define visited '())
  (define (iter l)
    (if (null? l)
      #f
      (let ((first (car l))
            (rest (cdr l)))
        (set! visited (cons l visited))
        (if (memq rest visited)
          #t
          (or (if (pair? first) (iter first) #f)
              (iter rest))))))
  (iter c))



(cyclic? '()) ; #f

(cyclic? '(a)) ; #f

(cyclic? '(a b)) ; #f

(define z (make-cycle (list 'a 'b 'c)))
(cyclic? z)

(define z2 '(a b c d))
(set-cdr! (last-pair z2) (cdr z2)) ; b <-> d
(cyclic? z2)

(define z3 '( (a b) (c d e f) (g h) i) )
(set-cdr! (cdr (cdr (cdr (cadr z3)))) (cdr (cdr (cadr z3))))  ; e <-> f
(cyclic? z3)


(define z4 '( (a b) (c d e f) (g h) i) )
(set-cdr! (cdr (caddr z4)) (cadr z4))  ; h -> d
(cyclic? z4)

(define z5 '( (a b) (c d e f) (g h) i) )
(set-cdr! (cdr (caddr z5)) (cdr z5))  ; h -> (c d e f)
(cyclic? z5)








