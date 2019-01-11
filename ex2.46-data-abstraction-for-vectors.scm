; A two-dimensional vector v running from the origin to a point can be represented as a pair consisting of an x-coordinate and a y-coordinate. Implement a data abstraction for vectors by giving a constructor make-vect and corresponding selectors xcor-vect and ycor-vect. In terms of your selectors and constructor, implement procedures add-vect, sub-vect, and scale-vect that perform the operations vector addition, vector subtraction, and multiplying a vector by a scalar:

(define (make-vect x y)
  (lambda (op) (op x y)))

(define (xcor-vect vect)
  (vect (lambda (x y) x)))

(define (ycor-vect vect)
  (vect (lambda (x y) y)))

(define (serilize-vect vect)
  (cons (xcor-vect vect) (ycor-vect vect)))

(define (add-vect vect1 vect2)
  (make-vect
    (+ (xcor-vect vect1)
       (xcor-vect vect2))
    (+ (ycor-vect vect1)
       (ycor-vect vect2))))

(define (scale-vect s vect)
  (make-vect
    (* s (xcor-vect vect))
    (* s (ycor-vect vect))))


(define (sub-vect vect1 vect2)
  (add-vect vect1 (scale-vect -1 vect2)))


(xcor-vect (make-vect 10.1 33))
(ycor-vect (make-vect 10.1 33))

(serilize-vect (add-vect (make-vect 10.1 33) (make-vect -1 40)))
(serilize-vect (sub-vect (make-vect 10.1 33) (make-vect -1 40)))

