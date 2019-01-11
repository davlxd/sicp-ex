(define (make-segment start-point end-point) (cons start-point end-point))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))




(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline)
  )

(define (print-segment s)
  (newline)
  (display "[")
  (display (start-segment s))
  (display ", ")
  (display (end-segment s))
  (display "]")
  (newline)
  )

; (print-point (make-point 10 30))
(print-segment (make-segment (make-point 10 30) (make-point 13 31)))


(define (midpoint-segment segment)
  (let ((start-point (start-segment segment))
	(end-point (end-segment segment)))
    (let ((start-x (x-point start-point))
	  (start-y (y-point start-point))
	  (end-x (x-point end-point))
	  (end-y (y-point end-point)))
      (define (average x y) (/ (+ x y) 2))
      (let ((mid-x (average start-x end-x))
	    (mid-y (average start-y end-y)))
	(make-point mid-x mid-y)))))



(print-point (midpoint-segment(make-segment (make-point 10 30) (make-point 13 31))))

