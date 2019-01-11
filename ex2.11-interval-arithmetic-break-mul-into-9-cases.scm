
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


(define (mul-interval1 x y)
  (let ((lx (lower-bound x))
	(ux (upper-bound x))
	(ly (lower-bound y))
	(uy (upper-bound y)))
    (cond ((and (>= lx 0) (>= ly 0)) (make-interval (* lx ly) (* ux uy)))
	  ((and (<= ux 0) (<= uy 0)) (make-interval (* ux uy) (* lx ly)))
	  ((and (<= ux 0) (and (<= ly 0) (>= uy 0))) (make-interval (* lx uy) (* lx ly)))
	  ((and (<= ux 0) (>= ly 0)) (make-interval (* lx uy) (* ux ly)))
	  ((and (and (<= lx 0) (>= ux 0)) (<= uy 0)) (make-interval (* ux ly) (* lx ly)))
	  ((and (and (<= lx 0) (>= ux 0)) (and (<= ly 0) (>= uy 0))) (make-interval (min (* ux ly) (* lx uy)) (* ux uy)))
	  ((and (and (<= lx 0) (>= ux 0)) (>= ly 0)) (make-interval (* lx uy) (* ux uy)))
	  ((and (>= lx 0) (<= uy 0)) (make-interval (* ux ly) (* lx uy)))
	  ((and (>= lx 0) (and (<= ly 0) (>= uy 0))) (make-interval (* ux ly) (* ux uy))))))


(let ((i1 (make-interval -1 -2)) (i2 (make-interval -2 3)))
  (display (mul-interval i1 i2)) (display (mul-interval1 i1 i2)))

(let ((i1 (make-interval -1 -2)) (i2 (make-interval -2 0)))
  (display (mul-interval i1 i2)) (display (mul-interval1 i1 i2)))

(let ((i1 (make-interval -1 0)) (i2 (make-interval 2 3)))
  (display (mul-interval i1 i2)) (display (mul-interval1 i1 i2)))

(let ((i1 (make-interval -1 5)) (i2 (make-interval -2 3)))
  (display (mul-interval i1 i2)) (display (mul-interval1 i1 i2)))

(let ((i1 (make-interval -1 5)) (i2 (make-interval 2 3)))
  (display (mul-interval i1 i2)) (display (mul-interval1 i1 i2)))

(let ((i1 (make-interval -1 5)) (i2 (make-interval -4 -1)))
  (display (mul-interval i1 i2)) (display (mul-interval1 i1 i2)))






