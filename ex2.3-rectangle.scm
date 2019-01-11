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


; Exercise 2.3.  Implement a representation for rectangles in a plane. (Hint: You may want to make use of exercise 2.2.) In terms of your constructors and selectors, create procedures that compute the perimeter and the area of a given rectangle. Now implement a different representation for rectangles. Can you design your system with suitable abstraction barriers, so that the same perimeter and area procedures will work using either representation?


(define (make-rect p1 p2) (cons p1 p2))

(define (edge-length-pair rect)
  (let ((p1 (car rect))
	(p2 (cdr rect)))
    (cons (abs (- (x-point p1) (x-point p2))) (abs (- (y-point p1) (y-point p2))))))

(define (perimeter rect)
  (let ((edge-pair (edge-length-pair rect)))
    (* 2 (+ (car edge-pair) (cdr edge-pair)))))

(display (perimeter (make-rect (make-point 1 2) (make-point 3 6))))
(newline)


(define (make-rect central-point x-axis-length y-axis-length)
  (cons central-point (cons x-axis-length y-axis-length)))

(define (edge-length-pair rect) (cdr rect))

(display (perimeter (make-rect (make-point 1 2) 4 12)))
(newline)

