; Again, assume nesting pair not count as a new pair

(define tracker '())
(define (add-to-tracker x) (set! tracker (cons x tracker)))
(define (clear-tracker) (set! tracker '()))
(define (tracker-contains? x) (not (not (memq x tracker))))

(define (count-pairs x)
  (cond ((not (pair? x)) 0)
        ((tracker-contains? x) 0)
        (else
          (add-to-tracker x)
          (+ (count-pairs (car x))
             (count-pairs (cdr x))
             1
             ))))

(count-pairs '())
(count-pairs '(a))
(count-pairs '(a b))
(count-pairs '(a b c) )



(define c (cons 'c '()))
(define C4 (cons 'a (cons c c)))
C4

(clear-tracker)
(count-pairs '(a (c) c) ) ; 4

(clear-tracker)
(count-pairs C4) ; 3



(define ab (cons 'a 'b))
(define C4 (cons 'c (cons ab ab))) ; 3
(clear-tracker)
(count-pairs C4)





(define c-null (cons 'c '())) ; aka '(c)
(define c-null-c-null (cons c-null c-null)) ; '((c) c)
(define C7 (cons c-null-c-null c-null-c-null)) ; '(((c) c) (c) c)
C7
(clear-tracker)
(count-pairs C7) ; 3
(clear-tracker)
(count-pairs '(((c) c) (c) c)) ; 7




(define ab (cons 'a 'b))
(define abab (cons ab ab))
(define C7 (cons abab abab))
C7
(clear-tracker)
(count-pairs C7)



