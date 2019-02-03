(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
; doesn't change too much, sligntly worse


; (define (adjoin-set x set)
;   (if (element-of-set? x set)
;     set
;     (cons x set)))
(define adjoin-set cons)
; from O(n) to O(1)


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
; doesn't change too much, slightly worse


; (define (union-set set1 set2)
;   (cond ((null? set1) set2)
; 	((element-of-set? (car set1) set2)
; 	 (union-set (cdr set1) set2))
; 	(else (union-set (cdr set1) (cons (car set1) set2)))))
(define union-set append)
; reduced from O(n^2) to O(n)

(union-set '(1 2 3) '(2 3 4))


; Application would be insert/union heavy scenarios


