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

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (upper-bound i) (max (car i) (cdr i)))
(define (lower-bound i) (min (car i) (cdr i)))


(define (sub-interval x y)
  (let ((new-lower-bound (- (lower-bound x) (upper-bound y)))
	(new-upper-bound (- (upper-bound x) (lower-bound y))))
    (make-interval new-lower-bound new-upper-bound)))

(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


; width of add:
; ->
(width-interval (add-interval x y)
		(/ (- (upper-bound (make-interval (+ (lower-bound x) (lower-bound y) (upper-bound x) (upper-bound y))))
		      (lower-bound (make-interval (+ (lower-bound x) (lower-bound y) (upper-bound x) (upper-bound y)))))
		   2))
; ->
(width-interval (add-interval x y)
		(/ (- (+ (upper-bound x) (upper-bound y))
		      (+ (lower-bound x) (lower-bound y)))
		   2))
; ->
(width-interval (add-interval x y)
		(/ (+ (- (upper-bound x) (lower-bound x))
		      (- (upper-bound y) (lower-bound y)))
		   2))
; ->
(width-interval (add-interval x y)
		(+ (/ (- (upper-bound x) (lower-bound x)) 2.0)
		   (/ (- (upper-bound y) (lower-bound y)) 2.0)))
;->
(width-interval (add-interval x y)
		(+ (width-interval x)
		   (width-interval y)))

; width of sub
(width-interval (sub-interval x y)
		(/ (- (upper-bound (make-interval (- (lower-bound x) (upper-bound y)) (- (upper-bound x) (lower-bound y))))
		      (lower-bound (make-interval (- (lower-bound x) (upper-bound y)) (- (upper-bound x) (lower-bound y)))))
		   2))
; ->
(width-interval (sub-interval x y)
		(/ (+ (- (upper-bound x) (lower-bound y))
		      (- (upper-bound y) (lower-bound x)))
		   2))

; -> ...

; width of multiply

(width-interval (mul-interval x y)
		(let ((p1 (* (lower-bound x) (lower-bound y)))
		      (p2 (* (lower-bound x) (upper-bound y)))
		      (p3 (* (upper-bound x) (lower-bound y)))
		      (p4 (* (upper-bound x) (upper-bound y))))
		  (/ (- (max p1 p2 p3 p4)
			(min p1 p2 p3 p4)))))

; e.g. [-3, -1] [-1, 1] -> [-3, 3]
;         2        2          6

; (max p1 p2 p3 p4) cannot expand to polynomial


