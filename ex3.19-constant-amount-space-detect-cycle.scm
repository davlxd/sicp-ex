; Floyd's Tortoise and Hare
; Not consider nested list for simplicity

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


(define (cyclic? c)
  (define (iter tortoise hare)
    (cond ((or (null? tortoise) (null? hare) (null? (cdr hare))) #f)
          ((eq? tortoise hare) #t)
          (else (iter (cdr tortoise) (cddr hare)))))
  (if (or (null? c) (null? (cdr c)))
    #f
    (iter (cdr c) (cddr c))))


(cyclic? '()) ; #f

(cyclic? '(a)) ; #f

(cyclic? '(a b)) ; #f

(define z (make-cycle (list 'a 'b 'c)))
(cyclic? z)

(define z2 '(a b c d))
(set-cdr! (last-pair z2) (cdr z2)) ; b <-> d
(cyclic? z2)

