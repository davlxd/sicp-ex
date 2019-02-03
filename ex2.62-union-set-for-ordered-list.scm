(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))


(define (union-set set1 set2)
  (define (iter set1 set2 result)
    (cond ((null? set1) (append (reverse set2) result))
          ((null? set2) (append (reverse set1) result))
          (else 
            (let ((x1 (car set1)) (x2 (car set2)))
              (cond ((= x1 x2)
                     (iter (cdr set1) (cdr set2) (cons x1 result)))
                    ((< x1 x2)
                     (iter (cdr set1) set2 (cons x1 result)))
                    ((> x1 x2)
                     (iter set1 (cdr set2) (cons x2 result))))))))
  (reverse (iter set1 set2 '())))


(define (union-set2 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else 
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set2 (cdr set1) (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set2 (cdr set1) set2)))
                  ((> x1 x2)
                   (cons x2 (union-set2 set1 (cdr set2)))))))))

(union-set '(1 3 5 7 9) '(2 4 5 6 7 10 12 14))


