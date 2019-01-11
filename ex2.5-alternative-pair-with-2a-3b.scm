(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))


(define (log1 base value)
  (/ (log value) (log base)))

(define (log2 base value)
  (define (iter value-so-far result)
    (if (= value-so-far value)
      result
      (iter (* base value-so-far) (+ result 1))))
  (iter 1 0))


(define (car z)
  (define (iter pow-of-2)
    (if (= 0 (remainder pow-of-2 3))
      (iter (/ pow-of-2 3))
      (log2 2 pow-of-2 )))
  (iter z))

(define (cdr z)
  (define (iter pow-of-3)
    (if (= 0 (remainder pow-of-3 2))
      (iter (/ pow-of-3 2))
      (log2 3 pow-of-3)))
  (iter z))

; Comment:
; This solution is direct, but not so efficient
; It divides out another base, then use log to calc the result
; 
; Solution at here http://community.schemewiki.org/?sicp-ex-2.5 is more efficient:
; increment result until combine its base the value cannot be mod=0

