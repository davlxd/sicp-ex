(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))
(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))


(define (sub-interval x y)
  (let ((new-lower-bound (- (lower-bound x) (upper-bound y)))
	(new-upper-bound (- (upper-bound x) (lower-bound y))))
    (make-interval new-lower-bound new-upper-bound)))

(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


; div [-1, 1], [3, 3]
; mul [-1. 1], [1/3, 1/3]
; [-1/3, 1/3]

(define (div-interval x y)
  (if (= (width-interval y) 0)
    (error "cannot divide 0")
    (mul-interval x
		  (make-interval (/ 1.0 (upper-bound y))
				 (/ 1.0 (lower-bound y))))))

; Spans 0, not span equals zero -_-||||||


