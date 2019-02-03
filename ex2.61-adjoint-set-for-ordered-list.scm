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

(define (adjoin-set x set)
  (define (iter before-in-rev set)
    (cond ((null? set) (reverse (cons x before-in-rev)))
          ((= x (car set)) (append (reverse before-in-rev) set))
          ((> x (car set)) (iter (cons (car set) before-in-rev) (cdr set)))
          ((< x (car set)) (append (reverse (cons x before-in-rev)) set))))
  (iter '() set))


; if x is smaller than all, then computing cost is one append operation with O(n)
; if x is bigger than all, then O(n) iteration, plus one reverse with O(n)
; on average, O(n/2) iteration, plus O(n/2) reverse, plus O(n/2) append
;

; recursive:

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))))

; 1 2 3 5 7

